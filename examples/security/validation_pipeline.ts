/**
 * JWT Validation Pipeline
 * 
 * This module demonstrates a comprehensive JWT validation pipeline with proper
 * error handling and defense-in-depth security measures.
 * 
 * Key security features:
 * 1. Signature verification before accessing token data
 * 2. Algorithm verification to prevent confusion attacks
 * 3. Required claims validation (exp, iat, iss, aud, sub, jti)
 * 4. Token replay prevention using a token blacklist
 * 5. Rate limiting for security-sensitive operations
 */

import * as jwt from 'jsonwebtoken';
import { createHash } from 'crypto';

// Interfaces for type safety
interface TokenValidationOptions {
  issuer: string | string[];
  audience: string | string[];
  algorithms: string[];
  clockTolerance?: number;
  maxTokenAge?: number;
}

interface JwtPayload {
  iss: string;        // Issuer
  sub: string;        // Subject (user ID)
  aud: string;        // Audience
  exp: number;        // Expiration time
  iat: number;        // Issued at time
  jti: string;        // JWT ID (unique identifier)
  [key: string]: any; // Additional claims
}

interface ValidationResult {
  valid: boolean;
  payload?: JwtPayload;
  error?: string;
}

// Basic in-memory token blacklist (use Redis/DB in production)
class TokenBlacklist {
  private blacklist: Map<string, number> = new Map();
  
  // Add a token to the blacklist until its expiration
  add(jti: string, expiration: number): void {
    this.blacklist.set(jti, expiration);
    this.cleanup();
  }
  
  // Check if a token is blacklisted
  isBlacklisted(jti: string): boolean {
    return this.blacklist.has(jti);
  }
  
  // Clean up expired tokens
  private cleanup(): void {
    const now = Math.floor(Date.now() / 1000);
    for (const [jti, expiration] of this.blacklist.entries()) {
      if (expiration < now) {
        this.blacklist.delete(jti);
      }
    }
  }
}

// Basic rate limiter for security-sensitive operations
class RateLimiter {
  private attempts: Map<string, { count: number, resetAt: number }> = new Map();
  private readonly maxAttempts: number;
  private readonly windowMs: number;
  
  constructor(maxAttempts: number = 10, windowMs: number = 60000) {
    this.maxAttempts = maxAttempts;
    this.windowMs = windowMs;
  }
  
  check(key: string): boolean {
    const now = Date.now();
    const record = this.attempts.get(key) || { count: 0, resetAt: now + this.windowMs };
    
    // Reset if window has passed
    if (record.resetAt <= now) {
      record.count = 1;
      record.resetAt = now + this.windowMs;
      this.attempts.set(key, record);
      return true;
    }
    
    // Increment and check
    record.count++;
    this.attempts.set(key, record);
    
    return record.count <= this.maxAttempts;
  }
}

export class JwtValidationPipeline {
  private readonly secretOrPublicKey: jwt.Secret | jwt.GetPublicKeyOrSecret;
  private readonly options: TokenValidationOptions;
  private readonly blacklist: TokenBlacklist;
  private readonly rateLimiter: RateLimiter;
  
  constructor(
    secretOrPublicKey: jwt.Secret | jwt.GetPublicKeyOrSecret,
    options: TokenValidationOptions,
    blacklist?: TokenBlacklist,
    rateLimiter?: RateLimiter
  ) {
    this.secretOrPublicKey = secretOrPublicKey;
    this.options = options;
    this.blacklist = blacklist || new TokenBlacklist();
    this.rateLimiter = rateLimiter || new RateLimiter();
  }
  
  /**
   * Validates a JWT token through a comprehensive security pipeline
   * 
   * @param token The JWT token to validate
   * @param requestIp The IP address of the requester (for rate limiting)
   * @returns Validation result with payload if valid
   */
  validate(token: string, requestIp?: string): ValidationResult {
    try {
      // 1. Basic structural validation
      if (!token || typeof token !== 'string') {
        return { valid: false, error: 'Invalid token format' };
      }
      
      // 2. Rate limiting check if IP provided
      if (requestIp && !this.rateLimiter.check(requestIp)) {
        return { valid: false, error: 'Rate limit exceeded' };
      }
      
      // 3. Extract algorithms from unverified header (for logging purposes only)
      let headerAlg: string;
      try {
        const decodedHeader = jwt.decode(token, { complete: true });
        if (!decodedHeader || typeof decodedHeader === 'string') {
          return { valid: false, error: 'Invalid token structure' };
        }
        headerAlg = decodedHeader.header.alg;
        
        // Check if algorithm in header is allowed (additional check before verification)
        if (!this.options.algorithms.includes(headerAlg)) {
          return { valid: false, error: `Algorithm ${headerAlg} is not allowed` };
        }
      } catch (e) {
        return { valid: false, error: 'Could not decode token header' };
      }
      
      // 4. Critical step: Verify signature and claims in one step
      // This is the most important security step - everything before this point
      // uses the unverified token, which could be manipulated
      const verificationOptions: jwt.VerifyOptions = {
        issuer: this.options.issuer,
        audience: this.options.audience,
        algorithms: this.options.algorithms,
        clockTolerance: this.options.clockTolerance || 0
      };
      
      const payload = jwt.verify(token, this.secretOrPublicKey, verificationOptions) as JwtPayload;
      
      // 5. Additional validation checks after signature verification
      
      // Check required claims
      if (!payload.jti) {
        return { valid: false, error: 'Token missing jti claim' };
      }
      
      if (!payload.sub) {
        return { valid: false, error: 'Token missing sub claim' };
      }
      
      // 6. Check token age if maxTokenAge is set
      if (this.options.maxTokenAge && payload.iat) {
        const now = Math.floor(Date.now() / 1000);
        if (now - payload.iat > this.options.maxTokenAge) {
          return { valid: false, error: 'Token exceeds maximum age' };
        }
      }
      
      // 7. Check if token is blacklisted (revoked)
      if (this.blacklist.isBlacklisted(payload.jti)) {
        return { valid: false, error: 'Token has been revoked' };
      }
      
      // 8. All validation passed
      return { valid: true, payload };
      
    } catch (error) {
      // Handle different types of JWT errors
      if (error instanceof jwt.JsonWebTokenError) {
        return { valid: false, error: 'Invalid token' };
      } else if (error instanceof jwt.NotBeforeError) {
        return { valid: false, error: 'Token not yet active' };
      } else if (error instanceof jwt.TokenExpiredError) {
        return { valid: false, error: 'Token expired' };
      } else {
        // Generic error handling without exposing details
        return { valid: false, error: 'Token validation failed' };
      }
    }
  }
  
  /**
   * Revokes a token by adding it to the blacklist
   * 
   * @param token The token to revoke
   * @returns Success indicator
   */
  revokeToken(token: string): boolean {
    try {
      // Only decode the token, don't verify it (we might want to revoke invalid tokens too)
      const decoded = jwt.decode(token, { complete: true });
      if (!decoded || typeof decoded === 'string') {
        return false;
      }
      
      const payload = decoded.payload as JwtPayload;
      if (!payload.jti || !payload.exp) {
        return false;
      }
      
      // Add to blacklist
      this.blacklist.add(payload.jti, payload.exp);
      return true;
    } catch (error) {
      return false;
    }
  }
  
  /**
   * Creates a secure hash of a token for logging purposes
   * Never log the actual token!
   * 
   * @param token The token to hash
   * @returns A SHA-256 hash of the token
   */
  createTokenHash(token: string): string {
    return createHash('sha256').update(token).digest('hex');
  }
}

// Example usage
async function example() {
  const publicKey = `-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu1SU1LfVLPHCozMxH2Mo
4lgOEePzNm0tRgeLezV6ffAt0gunVTLw7onLRnrq0/IzW7yWR7QkrmBL7jTKEn5u
+qKhbwKfBstIs+bMY2Zkp18gnTxKLxoS2tFczGkPLPgizskuemMghRniWaoLcyeh
kd3qqGElvW/VDL5AaWTg0nLVkjRo9z+40RQzuVaE8AkAFmxZzow3x+VJYKdjykkJ
0iT9wCS0DRTXu269V264Vf/3jvredZiKRkgwlL9xNAwxXFg0x/XFw005UWVRIkdg
cKWTjpBP2dPwVZ4WWC+9aGVd+Gyn1o0CLelf4rEjGoXbAAEgAqeGUxrcIlbjXfbc
mwIDAQAB
-----END PUBLIC KEY-----`;

  // Configure validation options
  const options: TokenValidationOptions = {
    issuer: 'https://auth.example.com',
    audience: 'https://api.example.com',
    algorithms: ['RS256'], // Only allow RS256
    clockTolerance: 30,    // 30 seconds tolerance for clock skew
    maxTokenAge: 3600 * 24 // Maximum 24 hours
  };

  // Create validation pipeline
  const pipeline = new JwtValidationPipeline(publicKey, options);

  // Validate a token
  const token = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMiwiZXhwIjoxNTE2MjQyNjIyLCJpc3MiOiJodHRwczovL2F1dGguZXhhbXBsZS5jb20iLCJhdWQiOiJodHRwczovL2FwaS5leGFtcGxlLmNvbSIsImp0aSI6ImFiYzEyMyJ9.THHNGg4AIq5Z4oNqyZbS-N13Uw9ONrxE7DePsG_aSblRPAhVVVr45qpS0CkdkdJrn2WrPpxpJCzO8vhHsZ05wBGwzgEcIsZQTJ4lI7LpZXf5Z8Z0Swqa6qLZl8kt0gQdRTwMSxpJtgUG0tVL3BNzR_nmIgKLpGpnlKieRUBWfnLxNr9V7vcq-aQjrV7SDTwlnhyr9nCvCCEPGX6K1dRZRRVfKRSHwkY9Cqy3GnswQI5lkLdnLZqQnDgeYw343y5I2xiOt4cheDFnGKTbwgAmWy-Xq1WvEK-SEJ6OUIlytxBwZ-Su-8DU1jCu08aR9HD6zljQV_yglLhKpJioYtHO5Q';

  const result = pipeline.validate(token, '192.168.1.1');
  
  if (result.valid && result.payload) {
    console.log('Token is valid');
    console.log('User ID:', result.payload.sub);
    console.log('Is admin:', result.payload.admin);
  } else {
    console.log('Token validation failed:', result.error);
  }
  
  // Example of token revocation
  const revoked = pipeline.revokeToken(token);
  console.log('Token revoked:', revoked);
  
  // After revocation, the token should be invalid
  const resultAfterRevocation = pipeline.validate(token);
  console.log('Token still valid after revocation:', resultAfterRevocation.valid);
  
  // Safe token hash for logging
  const tokenHash = pipeline.createTokenHash(token);
  console.log('Token hash for logging:', tokenHash);
}

// Only run example when directly executed
if (require.main === module) {
  example().catch(console.error);
}