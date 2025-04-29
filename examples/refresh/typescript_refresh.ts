import * as jwt from 'jsonwebtoken';
import * as crypto from 'crypto';
import { RedisClientType } from 'redis';

// Configuration
const SECRET_KEY = process.env.JWT_SECRET || 'your-secret-key';
const REFRESH_SECRET = process.env.REFRESH_SECRET || 'your-refresh-secret';

interface AccessTokenPayload {
  sub: string;        // User ID
  iat: number;        // Issued at time
  exp: number;        // Expiration time
  jti: string;        // JWT ID
  roles: string[];    // User roles
  permissions: string[]; // Specific permissions
}

interface RefreshTokenPayload {
  sub: string;        // User ID
  iat: number;        // Issued at time
  exp: number;        // Expiration time
  jti: string;        // JWT ID
  type: 'refresh';    // Token type
}

interface UserData {
  roles: string[];
  permissions: string[];
}

interface TokenPair {
  access_token: string;
  token_type: string;
  expires_in: number;
  refresh_token: string;
  refresh_expires_in: number;
}

interface TokenRefreshResult {
  access_token?: string;
  token_type?: string;
  expires_in?: number;
  error?: string;
}

interface TokenRevokeResult {
  success?: boolean;
  message?: string;
  error?: string;
}

class TokenService {
  private redisClient: RedisClientType;
  private accessTtl: number = 900;    // 15 minutes
  private refreshTtl: number = 2592000; // 30 days
  
  constructor(redisClient: RedisClientType) {
    this.redisClient = redisClient;
  }
  
  /**
   * Generate a new access+refresh token pair
   */
  public async generateTokenPair(
    userId: string,
    roles: string[],
    permissions: string[]
  ): Promise<TokenPair> {
    const now = Math.floor(Date.now() / 1000);
    
    // Create a unique refresh token ID
    const refreshJti = crypto.randomUUID();
    
    // Access token payload
    const accessPayload: AccessTokenPayload = {
      sub: userId,
      iat: now,
      exp: now + this.accessTtl,
      jti: crypto.randomUUID(),
      roles,
      permissions
    };
    
    // Refresh token payload
    const refreshPayload: RefreshTokenPayload = {
      sub: userId,
      iat: now,
      exp: now + this.refreshTtl,
      jti: refreshJti,
      type: 'refresh'
    };
    
    // Create tokens
    const accessToken = jwt.sign(accessPayload, SECRET_KEY, { algorithm: 'HS256' });
    const refreshToken = jwt.sign(refreshPayload, REFRESH_SECRET, { algorithm: 'HS256' });
    
    // Store refresh token in Redis for validation/revocation
    await this.storeRefreshToken(refreshJti, userId, now + this.refreshTtl);
    
    return {
      access_token: accessToken,
      token_type: 'Bearer',
      expires_in: this.accessTtl,
      refresh_token: refreshToken,
      refresh_expires_in: this.refreshTtl
    };
  }
  
  /**
   * Use a refresh token to generate a new access token
   */
  public async refreshAccessToken(refreshToken: string): Promise<TokenRefreshResult> {
    try {
      // Verify the refresh token
      const decoded = jwt.verify(refreshToken, REFRESH_SECRET) as RefreshTokenPayload;
      
      // Check if it's a refresh token
      if (decoded.type !== 'refresh') {
        return { error: 'Invalid token type' };
      }
      
      // Check if token has been revoked
      const isValid = await this.validateRefreshToken(decoded.jti);
      if (!isValid) {
        return { error: 'Token has been revoked' };
      }
      
      // Get user data (in a real app, you'd get the latest roles/permissions)
      const userId = decoded.sub;
      const userData = await this.getUserData(userId);
      
      // Generate a new access token
      const now = Math.floor(Date.now() / 1000);
      const newAccessPayload: AccessTokenPayload = {
        sub: userId,
        iat: now,
        exp: now + this.accessTtl,
        jti: crypto.randomUUID(),
        roles: userData.roles,
        permissions: userData.permissions
      };
      
      const newAccessToken = jwt.sign(newAccessPayload, SECRET_KEY, { algorithm: 'HS256' });
      
      return {
        access_token: newAccessToken,
        token_type: 'Bearer',
        expires_in: this.accessTtl
      };
      
    } catch (error) {
      if (error instanceof jwt.TokenExpiredError) {
        return { error: 'Refresh token expired' };
      }
      return { error: 'Invalid token' };
    }
  }
  
  /**
   * Revoke a refresh token
   */
  public async revokeRefreshToken(refreshToken: string): Promise<TokenRevokeResult> {
    try {
      const decoded = jwt.verify(refreshToken, REFRESH_SECRET) as RefreshTokenPayload;
      await this.revokeRefreshTokenById(decoded.jti);
      return { success: true, message: 'Token revoked' };
    } catch (error) {
      return { error: 'Invalid token' };
    }
  }
  
  /**
   * Store refresh token metadata in Redis
   */
  private async storeRefreshToken(jti: string, userId: string, expiry: number): Promise<void> {
    // Key format: refresh_token:{jti}
    const key = `refresh_token:${jti}`;
    
    await this.redisClient.hSet(key, {
      user_id: userId,
      created_at: Math.floor(Date.now() / 1000).toString(),
      revoked: 'false'
    });
    
    // Set expiration
    await this.redisClient.expireAt(key, expiry);
  }
  
  /**
   * Check if a refresh token is valid and not revoked
   */
  private async validateRefreshToken(jti: string): Promise<boolean> {
    const key = `refresh_token:${jti}`;
    // Check if token exists and is not revoked
    const tokenData = await this.redisClient.hGetAll(key);
    return !!tokenData && tokenData.revoked === 'false';
  }
  
  /**
   * Mark a refresh token as revoked
   */
  private async revokeRefreshTokenById(jti: string): Promise<void> {
    const key = `refresh_token:${jti}`;
    await this.redisClient.hSet(key, 'revoked', 'true');
  }
  
  /**
   * Get latest user data (roles/permissions)
   */
  private async getUserData(userId: string): Promise<UserData> {
    // In a real app, you would fetch this from your database
    // This is a mockup for demonstration
    return {
      roles: ['user', 'subscriber'],
      permissions: ['read:content', 'post:comments']
    };
  }
}

// Example usage in an Express app:
/*
import express from 'express';
import { createClient } from 'redis';

const app = express();
app.use(express.json());

// Initialize Redis client
const redisClient = createClient({
  url: process.env.REDIS_URL || 'redis://localhost:6379'
});
redisClient.connect().catch(console.error);

// Initialize token service
const tokenService = new TokenService(redisClient);

// Login endpoint
app.post('/api/auth/login', async (req, res) => {
  // Validate credentials (not shown)
  const { username, password } = req.body;
  
  // Get user from database (not shown)
  const user = { id: 'user123', roles: ['user'], permissions: ['read:content'] };
  
  // Generate tokens
  const tokenPair = await tokenService.generateTokenPair(
    user.id,
    user.roles,
    user.permissions
  );
  
  res.json(tokenPair);
});

// Refresh token endpoint
app.post('/api/auth/refresh', async (req, res) => {
  const { refresh_token } = req.body;
  
  if (!refresh_token) {
    return res.status(400).json({ error: 'Refresh token is required' });
  }
  
  const result = await tokenService.refreshAccessToken(refresh_token);
  
  if (result.error) {
    return res.status(401).json({ error: result.error });
  }
  
  res.json(result);
});

// Logout endpoint
app.post('/api/auth/logout', async (req, res) => {
  const { refresh_token } = req.body;
  
  if (!refresh_token) {
    return res.status(400).json({ error: 'Refresh token is required' });
  }
  
  const result = await tokenService.revokeRefreshToken(refresh_token);
  
  if (result.error) {
    return res.status(401).json({ error: result.error });
  }
  
  res.json(result);
});

app.listen(3000, () => {
  console.log('Server running on port 3000');
});
*/
