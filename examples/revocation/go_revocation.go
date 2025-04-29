package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/golang-jwt/jwt/v4"
	"github.com/google/uuid"
)

// Configuration
var (
	secretKey    = getEnv("JWT_SECRET", "your-secret-key")
	redisAddr    = getEnv("REDIS_ADDR", "localhost:6379")
	redisPwd     = getEnv("REDIS_PWD", "")
	redisDB      = 0
	accessTTL    = 15 * time.Minute
	revokePrefix = "revoked_token:"
)

// Custom claims with a family identifier for token revocation
type CustomClaims struct {
	jwt.RegisteredClaims
	Roles       []string          `json:"roles"`
	Permissions []string          `json:"permissions"`
	Family      string            `json:"fam,omitempty"` // Token family identifier
	Metadata    map[string]string `json:"meta,omitempty"`
}

// RevocationService manages token revocation
type RevocationService struct {
	redisClient *redis.Client
	ctx         context.Context
}

// NewRevocationService creates a new token revocation service
func NewRevocationService() *RevocationService {
	redisClient := redis.NewClient(&redis.Options{
		Addr:     redisAddr,
		Password: redisPwd,
		DB:       redisDB,
	})

	return &RevocationService{
		redisClient: redisClient,
		ctx:         context.Background(),
	}
}

// Close closes the Redis connection
func (s *RevocationService) Close() {
	s.redisClient.Close()
}

// IsTokenRevoked checks if a token is revoked
func (s *RevocationService) IsTokenRevoked(tokenID string) (bool, error) {
	key := revokePrefix + tokenID
	val, err := s.redisClient.Exists(s.ctx, key).Result()
	if err != nil {
		return false, fmt.Errorf("failed to check revocation status: %w", err)
	}
	return val > 0, nil
}

// RevokeToken revokes a token by its ID
func (s *RevocationService) RevokeToken(tokenID string, expiresAt time.Time) error {
	key := revokePrefix + tokenID
	// Store until token expiration (to avoid Redis growing indefinitely)
	ttl := time.Until(expiresAt)
	if ttl <= 0 {
		return nil // Already expired, no need to revoke
	}

	_, err := s.redisClient.Set(s.ctx, key, "1", ttl).Result()
	if err != nil {
		return fmt.Errorf("failed to revoke token: %w", err)
	}
	return nil
}

// RevokeTokenFamily revokes all tokens in a family
func (s *RevocationService) RevokeTokenFamily(family string, until time.Time) error {
	key := revokePrefix + "family:" + family
	// Store family revocation with TTL
	ttl := time.Until(until)
	if ttl <= 0 {
		ttl = 24 * 30 * time.Hour // Default to 30 days if until is in the past
	}

	_, err := s.redisClient.Set(s.ctx, key, time.Now().Unix(), ttl).Result()
	if err != nil {
		return fmt.Errorf("failed to revoke token family: %w", err)
	}
	return nil
}

// IsTokenFamilyRevoked checks if a token family is revoked
func (s *RevocationService) IsTokenFamilyRevoked(family string, issuedAt time.Time) (bool, error) {
	key := revokePrefix + "family:" + family
	val, err := s.redisClient.Get(s.ctx, key).Result()
	if err == redis.Nil {
		return false, nil // Not revoked
	}
	if err != nil {
		return false, fmt.Errorf("failed to check family revocation: %w", err)
	}

	// Check if token was issued before revocation
	var revokedAt int64
	if err := json.Unmarshal([]byte(val), &revokedAt); err != nil {
		revokedAt, err = parseInt64(val)
		if err != nil {
			return false, fmt.Errorf("invalid revocation timestamp: %w", err)
		}
	}

	return issuedAt.Before(time.Unix(revokedAt, 0)), nil
}

// TokenService handles JWT token operations
type TokenService struct {
	revocationService *RevocationService
}

// NewTokenService creates a new token service
func NewTokenService(revocationService *RevocationService) *TokenService {
	return &TokenService{
		revocationService: revocationService,
	}
}

// CreateToken generates a new JWT token
func (s *TokenService) CreateToken(userID string, roles, permissions []string) (string, error) {
	now := time.Now()
	expiresAt := now.Add(accessTTL)
	tokenID := uuid.New().String()
	
	// Generate a family identifier for related tokens (useful for revocation)
	family := generateTokenFamily(userID)

	claims := CustomClaims{
		RegisteredClaims: jwt.RegisteredClaims{
			Subject:   userID,
			ExpiresAt: jwt.NewNumericDate(expiresAt),
			IssuedAt:  jwt.NewNumericDate(now),
			NotBefore: jwt.NewNumericDate(now),
			ID:        tokenID,
		},
		Roles:       roles,
		Permissions: permissions,
		Family:      family,
		Metadata: map[string]string{
			"ip": "192.168.1.100", // Would be actual client IP in real app
		},
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	signedToken, err := token.SignedString([]byte(secretKey))
	if err != nil {
		return "", fmt.Errorf("failed to sign token: %w", err)
	}

	return signedToken, nil
}

// VerifyToken verifies and parses a token
func (s *TokenService) VerifyToken(tokenString string) (*CustomClaims, error) {
	token, err := jwt.ParseWithClaims(tokenString, &CustomClaims{}, func(token *jwt.Token) (interface{}, error) {
		// Verify signing method
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return []byte(secretKey), nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to parse token: %w", err)
	}

	if !token.Valid {
		return nil, fmt.Errorf("invalid token")
	}

	claims, ok := token.Claims.(*CustomClaims)
	if !ok {
		return nil, fmt.Errorf("invalid claims type")
	}

	// Check if token is revoked
	isRevoked, err := s.revocationService.IsTokenRevoked(claims.ID)
	if err != nil {
		return nil, fmt.Errorf("failed to check revocation: %w", err)
	}
	if isRevoked {
		return nil, fmt.Errorf("token is revoked")
	}

	// Check if token family is revoked
	if claims.Family != "" {
		isFamilyRevoked, err := s.revocationService.IsTokenFamilyRevoked(
			claims.Family,
			claims.IssuedAt.Time,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to check family revocation: %w", err)
		}
		if isFamilyRevoked {
			return nil, fmt.Errorf("token family is revoked")
		}
	}

	return claims, nil
}

// RevokeToken revokes a specific token
func (s *TokenService) RevokeToken(tokenString string) error {
	token, err := jwt.ParseWithClaims(tokenString, &CustomClaims{}, func(token *jwt.Token) (interface{}, error) {
		return []byte(secretKey), nil
	})

	if err != nil {
		return fmt.Errorf("failed to parse token: %w", err)
	}

	claims, ok := token.Claims.(*CustomClaims)
	if !ok {
		return fmt.Errorf("invalid claims type")
	}

	return s.revocationService.RevokeToken(claims.ID, claims.ExpiresAt.Time)
}

// RevokeAllUserTokens revokes all tokens for a user
func (s *TokenService) RevokeAllUserTokens(userID string) error {
	family := generateTokenFamily(userID)
	// Revoke for 30 days (or longer if needed)
	return s.revocationService.RevokeTokenFamily(
		family,
		time.Now().Add(30*24*time.Hour),
	)
}

// Helper functions

// getEnv gets an environment variable or returns a default
func getEnv(key, fallback string) string {
	if value, exists := os.LookupEnv(key); exists {
		return value
	}
	return fallback
}

// generateTokenFamily creates a unique family identifier for a user
func generateTokenFamily(userID string) string {
	hash := sha256.Sum256([]byte(userID))
	return hex.EncodeToString(hash[:])
}

// parseInt64 parses a string to int64
func parseInt64(s string) (int64, error) {
	var n int64
	_, err := fmt.Sscanf(s, "%d", &n)
	return n, err
}

func main() {
	// Example usage
	revocationService := NewRevocationService()
	defer revocationService.Close()

	tokenService := NewTokenService(revocationService)

	// Create a token
	userID := "user123"
	roles := []string{"user", "admin"}
	permissions := []string{"read:users", "write:users"}

	token, err := tokenService.CreateToken(userID, roles, permissions)
	if err != nil {
		log.Fatalf("Failed to create token: %v", err)
	}
	fmt.Printf("Token created: %s\n\n", token)

	// Verify the token
	claims, err := tokenService.VerifyToken(token)
	if err != nil {
		log.Fatalf("Failed to verify token: %v", err)
	}
	fmt.Printf("Token verified. User: %s, Roles: %v\n\n", claims.Subject, claims.Roles)

	// Revoke the token
	err = tokenService.RevokeToken(token)
	if err != nil {
		log.Fatalf("Failed to revoke token: %v", err)
	}
	fmt.Println("Token revoked")

	// Try to verify the revoked token
	_, err = tokenService.VerifyToken(token)
	fmt.Printf("Verification after revocation: %v\n\n", err)

	// Revoke all user tokens
	err = tokenService.RevokeAllUserTokens(userID)
	if err != nil {
		log.Fatalf("Failed to revoke all user tokens: %v", err)
	}
	fmt.Println("All user tokens revoked")

	// Create a new token and try to verify (should fail due to family revocation)
	newToken, _ := tokenService.CreateToken(userID, roles, permissions)
	_, err = tokenService.VerifyToken(newToken)
	fmt.Printf("Verification of new token after family revocation: %v\n", err)
}
