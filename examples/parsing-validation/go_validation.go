package main

import (
	"fmt"
	"time"

	"github.com/golang-jwt/jwt/v4"
)

// CustomClaims extends standard JWT claims with your application-specific claims
type CustomClaims struct {
	jwt.RegisteredClaims
	Role        string   `json:"role"`
	Permissions []string `json:"permissions"`
}

// JWTValidator handles JWT validation with proper security checks
type JWTValidator struct {
	secretKey []byte
	issuer    string
	audience  string
}

// NewJWTValidator creates a new JWT validator
func NewJWTValidator(secretKey, issuer, audience string) *JWTValidator {
	return &JWTValidator{
		secretKey: []byte(secretKey),
		issuer:    issuer,
		audience:  audience,
	}
}

// ValidateToken properly validates a JWT token
func (v *JWTValidator) ValidateToken(tokenString string) (*CustomClaims, error) {
	// Parse the token
	token, err := jwt.ParseWithClaims(tokenString, &CustomClaims{}, func(token *jwt.Token) (interface{}, error) {
		// Validate the signing algorithm
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		
		return v.secretKey, nil
	})
	
	if err != nil {
		return nil, fmt.Errorf("token parsing failed: %w", err)
	}
	
	if !token.Valid {
		return nil, fmt.Errorf("invalid token")
	}
	
	claims, ok := token.Claims.(*CustomClaims)
	if !ok {
		return nil, fmt.Errorf("invalid claims type")
	}
	
	// Explicitly validate required claims
	if err := v.validateClaims(claims); err != nil {
		return nil, err
	}
	
	return claims, nil
}

// validateClaims validates the required claims in the token
func (v *JWTValidator) validateClaims(claims *CustomClaims) error {
	now := time.Now()
	
	// Validate expiration
	if claims.ExpiresAt == nil {
		return fmt.Errorf("token missing expiration time")
	}
	if claims.ExpiresAt.Before(now) {
		return fmt.Errorf("token expired")
	}
	
	// Validate issued at
	if claims.IssuedAt == nil {
		return fmt.Errorf("token missing issued at time")
	}
	// Allow 5 minutes of clock skew
	if claims.IssuedAt.After(now.Add(5 * time.Minute)) {
		return fmt.Errorf("token issued in the future")
	}
	
	// Validate issuer
	if claims.Issuer != v.issuer {
		return fmt.Errorf("invalid issuer")
	}
	
	// Validate audience
	if claims.Audience == nil || !claims.Audience.Contains(v.audience) {
		return fmt.Errorf("invalid audience")
	}
	
	// Validate subject
	if claims.Subject == "" {
		return fmt.Errorf("token missing subject")
	}
	
	// Custom validations
	if claims.Role == "" {
		return fmt.Errorf("token missing required role claim")
	}
	
	return nil
}

// UnsafelyExtractClaims demonstrates why parsing without verification is dangerous
// NEVER use this for authentication decisions!
func UnsafelyExtractClaims(tokenString string) (*CustomClaims, error) {
	parts := strings.Split(tokenString, ".")
	if len(parts) != 3 {
		return nil, fmt.Errorf("invalid token format")
	}
	
	// Decode the claims part (middle segment)
	claimBytes, err := jwt.DecodeSegment(parts[1])
	if err != nil {
		return nil, fmt.Errorf("failed to decode claims: %w", err)
	}
	
	var claims CustomClaims
	if err := json.Unmarshal(claimBytes, &claims); err != nil {
		return nil, fmt.Errorf("failed to unmarshal claims: %w", err)
	}
	
	// WARNING: No verification has been performed!
	return &claims, nil
}

// SecurityTestScenarios demonstrates different security test cases
func SecurityTestScenarios() {
	fmt.Println("\n--- SECURITY TEST SCENARIOS ---")
	
	// Create a validator
	validator := NewJWTValidator("your-secret-key", "auth-service", "api-gateway")
	
	// Test case 1: Expired token
	expiredTokenString := createExpiredToken()
	fmt.Println("Testing expired token:")
	if _, err := validator.ValidateToken(expiredTokenString); err != nil {
		fmt.Printf("✅ Security check passed: %s\n", err)
	} else {
		fmt.Println("❌ Security check failed: Accepted expired token!")
	}
	
	// Test case 2: Wrong issuer
	wrongIssuerTokenString := createTokenWithWrongIssuer()
	fmt.Println("\nTesting token with wrong issuer:")
	if _, err := validator.ValidateToken(wrongIssuerTokenString); err != nil {
		fmt.Printf("✅ Security check passed: %s\n", err)
	} else {
		fmt.Println("❌ Security check failed: Accepted token with wrong issuer!")
	}
	
	// Test case 3: None algorithm attack
	noneAlgorithmTokenString := createNoneAlgorithmToken()
	fmt.Println("\nTesting 'none' algorithm attack:")
	if _, err := validator.ValidateToken(noneAlgorithmTokenString); err != nil {
		fmt.Printf("✅ Security check passed: %s\n", err)
	} else {
		fmt.Println("❌ Security check failed: Accepted 'none' algorithm token!")
	}
	
	// Test case 4: Tampered payload
	tamperedTokenString := createTamperedToken()
	fmt.Println("\nTesting tampered token:")
	if _, err := validator.ValidateToken(tamperedTokenString); err != nil {
		fmt.Printf("✅ Security check passed: %s\n", err)
	} else {
		fmt.Println("❌ Security check failed: Accepted tampered token!")
	}
}

func main() {
	// Example usage
	validator := NewJWTValidator("your-secret-key", "auth-service", "api-gateway")
	
	// This would be a token from a request in a real application
	tokenString := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
	
	claims, err := validator.ValidateToken(tokenString)
	if err != nil {
		fmt.Println("Token validation failed:", err)
		return
	}
	
	fmt.Println("Token is valid!")
	fmt.Println("Subject:", claims.Subject)
	fmt.Println("Role:", claims.Role)
	fmt.Println("Permissions:", claims.Permissions)
	
	// Run security test scenarios
	SecurityTestScenarios()
}
