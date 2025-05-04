/**
 * Secure JWT Parsing Implementation in TypeScript
 * 
 * This module demonstrates secure-by-default JWT handling practices:
 * 1. Signature verification BEFORE payload access
 * 2. Explicit algorithm whitelisting
 * 3. Comprehensive claim validation
 * 4. Secure error handling
 * 5. Strong typing
 */

import * as crypto from 'crypto';

// Define strong types for JWT components
interface JWTHeader {
  alg: string;
  typ: string;
  kid?: string; // Optional key ID
}

interface JWTClaims {
  iss?: string; // Issuer
  sub?: string; // Subject
  aud?: string | string[]; // Audience
  exp?: number; // Expiration time
  nbf?: number; // Not before time
  iat?: number; // Issued at time
  jti?: string; // JWT ID
  [key: string]: any; // Allow additional claims
}

interface VerifierOptions {
  issuer?: string;
  audience?: string | string[];
  clockTolerance?: number; // in seconds
  maxTokenAge?: number; // in seconds
}

// Custom error class that doesn't leak information through specific types
class JWTVerificationError extends Error {
  private details: string;

  constructor(message: string, details: string) {
    super(message);
    this.name = 'JWTVerificationError';
    this.details = details; // Private field - not exposed but available for logging
  }

  // Method for internal logging only
  getDetails(): string {
    return this.details;
  }
}

/**
 * Secure JWT Token Verifier
 * 
 * Implements secure-by-default approach where signature verification
 * MUST occur before payload access.
 */
export class TokenVerifier {
  // Explicitly whitelist allowed algorithms - frozen to prevent modification
  private static readonly ALLOWED_ALGORITHMS = Object.freeze(['HS256', 'RS256', 'ES256']);
  
  private keys: Map<string, string | Buffer>;
  private options: VerifierOptions;

  /**
   * Create a new token verifier
   * 
   * @param keys - Map of key IDs to cryptographic keys
   * @param options - Verification options
   */
  constructor(
    keys: Map<string, string | Buffer> | Record<string, string | Buffer>,
    options: VerifierOptions = {}
  ) {
    this.keys = keys instanceof Map ? keys : new Map(Object.entries(keys));
    this.options = options;
  }

  /**
   * Verify and decode a JWT token securely
   * 
   * This method follows security best practices:
   * 1. Parse the token into parts without decoding payload
   * 2. Verify signature before accessing claims
   * 3. Validate token metadata (exp, nbf, iss, aud)
   * 4. Only then return the decoded payload
   * 
   * @param token - The JWT token string
   * @returns Tuple containing the verified header and claims
   * @throws JWTVerificationError if token is invalid for any reason
   */
  public verifyAndDecode(token: string): [JWTHeader, JWTClaims] {
    try {
      // Split the token into parts
      const parts = token.split('.');
      if (parts.length !== 3) {
        throw new JWTVerificationError(
          'Invalid token format', 
          'Token does not have three parts'
        );
      }

      const [headerPart, payloadPart, signaturePart] = parts;

      // Decode header first (minimal parsing needed for verification)
      const header = this.decodeBase64UrlJson<JWTHeader>(headerPart);

      // Verify algorithm is supported (prevent algorithm confusion)
      if (!header.alg || !TokenVerifier.ALLOWED_ALGORITHMS.includes(header.alg)) {
        throw new JWTVerificationError(
          'Invalid algorithm', 
          `Algorithm ${header.alg} is not allowed`
        );
      }

      // Verify signature first (before decoding payload)
      this.verifySignature(
        `${headerPart}.${payloadPart}`,
        signaturePart,
        header.alg,
        header.kid
      );

      // Only decode payload after signature verification
      const payload = this.decodeBase64UrlJson<JWTClaims>(payloadPart);

      // Validate standard claims
      this.validateClaims(payload);

      return [header, payload];
    } catch (error) {
      if (error instanceof JWTVerificationError) {
        // Re-throw our custom errors
        throw error;
      } else {
        // Convert unknown errors to our error type with generic message
        // to prevent information leakage, but preserve details for logging
        console.error('Token validation error:', error);
        throw new JWTVerificationError(
          'Invalid token',
          `Unexpected error: ${error instanceof Error ? error.message : String(error)}`
        );
      }
    }
  }

  /**
   * Decode base64url-encoded JSON safely
   * 
   * @param data - Base64url-encoded JSON string
   * @returns Decoded JSON as object
   * @throws Error if decoding fails
   */
  private decodeBase64UrlJson<T>(data: string): T {
    // Add padding if needed
    const paddingNeeded = data.length % 4;
    let padded = data;
    if (paddingNeeded) {
      padded += '='.repeat(4 - paddingNeeded);
    }

    // Replace URL-safe characters
    const base64 = padded.replace(/-/g, '+').replace(/_/g, '/');

    // Decode and parse JSON
    const decodedStr = Buffer.from(base64, 'base64').toString('utf-8');
    return JSON.parse(decodedStr);
  }

  /**
   * Verify the token signature
   * 
   * @param message - The message to verify (header.payload)
   * @param signatureB64 - Base64url-encoded signature
   * @param algorithm - Algorithm used for signing
   * @param keyId - Optional key identifier
   * @throws JWTVerificationError if signature verification fails
   */
  private verifySignature(
    message: string,
    signatureB64: string,
    algorithm: string,
    keyId?: string
  ): void {
    // Add padding if needed
    const paddingNeeded = signatureB64.length % 4;
    let padded = signatureB64;
    if (paddingNeeded) {
      padded += '='.repeat(4 - paddingNeeded);
    }

    // Replace URL-safe characters
    const base64 = padded.replace(/-/g, '+').replace(/_/g, '/');

    // Decode signature
    let signature: Buffer;
    try {
      signature = Buffer.from(base64, 'base64');
    } catch (error) {
      throw new JWTVerificationError(
        'Invalid signature format',
        `Base64 decoding error: ${error instanceof Error ? error.message : String(error)}`
      );
    }

    // Get key based on key ID, or use default key
    const key = this.keys.get(keyId || 'default');
    if (!key) {
      throw new JWTVerificationError('Key not found', `Key ID not found: ${keyId || 'default'}`);
    }

    // Verify signature using appropriate algorithm
    let isValid = false;

    if (algorithm === 'HS256') {
      // HMAC-SHA256 verification
      const hmac = crypto.createHmac('sha256', key);
      hmac.update(message);
      const expected = hmac.digest();
      
      // Use constant-time comparison to prevent timing attacks
      isValid = crypto.timingSafeEqual(signature, expected);
    } else if (algorithm === 'RS256') {
      // RSA-SHA256 verification
      try {
        const verify = crypto.createVerify('RSA-SHA256');
        verify.update(message);
        isValid = verify.verify(key, signature);
      } catch (error) {
        throw new JWTVerificationError(
          'Invalid signature',
          `RSA verification error: ${error instanceof Error ? error.message : String(error)}`
        );
      }
    } else if (algorithm === 'ES256') {
      // ECDSA-SHA256 verification
      try {
        const verify = crypto.createVerify('SHA256');
        verify.update(message);
        isValid = verify.verify(key, signature);
      } catch (error) {
        throw new JWTVerificationError(
          'Invalid signature',
          `ECDSA verification error: ${error instanceof Error ? error.message : String(error)}`
        );
      }
    } else {
      // This should never happen due to earlier check
      throw new JWTVerificationError(
        'Unsupported algorithm',
        `Algorithm not implemented: ${algorithm}`
      );
    }

    if (!isValid) {
      throw new JWTVerificationError('Invalid signature', 'Signature verification failed');
    }
  }

  /**
   * Validate standard claims
   * 
   * @param payload - The decoded token payload
   * @throws JWTVerificationError if any claim validation fails
   */
  private validateClaims(payload: JWTClaims): void {
    const now = Math.floor(Date.now() / 1000);
    const tolerance = this.options.clockTolerance || 0;

    // Check expiration
    if (payload.exp !== undefined) {
      if (now > payload.exp + tolerance) {
        throw new JWTVerificationError(
          'Token expired',
          `Token expired at ${new Date(payload.exp * 1000).toISOString()}`
        );
      }
    }

    // Check not-before time
    if (payload.nbf !== undefined) {
      if (now < payload.nbf - tolerance) {
        throw new JWTVerificationError(
          'Token not yet valid',
          `Token valid from ${new Date(payload.nbf * 1000).toISOString()}`
        );
      }
    }

    // Check max age if specified
    if (this.options.maxTokenAge !== undefined && payload.iat !== undefined) {
      const maxAge = this.options.maxTokenAge;
      if (now > payload.iat + maxAge) {
        throw new JWTVerificationError(
          'Token too old',
          `Token issued at ${new Date(payload.iat * 1000).toISOString()} exceeds max age`
        );
      }
    }

    // Check issuer
    if (this.options.issuer !== undefined && payload.iss !== this.options.issuer) {
      throw new JWTVerificationError(
        'Invalid issuer',
        `Expected ${this.options.issuer}, got ${payload.iss}`
      );
    }

    // Check audience
    if (this.options.audience !== undefined && payload.aud !== undefined) {
      const audience = this.options.audience;
      const tokenAud = payload.aud;

      let validAudience = false;

      if (typeof audience === 'string') {
        if (typeof tokenAud === 'string') {
          validAudience = audience === tokenAud;
        } else {
          validAudience = tokenAud.includes(audience);
        }
      } else { // audience is string[]
        if (typeof tokenAud === 'string') {
          validAudience = audience.includes(tokenAud);
        } else {
          validAudience = tokenAud.some(aud => audience.includes(aud));
        }
      }

      if (!validAudience) {
        throw new JWTVerificationError(
          'Invalid audience',
          `Token audience ${JSON.stringify(tokenAud)} does not match ${JSON.stringify(audience)}`
        );
      }
    }
  }
}

// Example usage
function exampleUsage() {
  // Sample token (normally would come from Authorization header)
  const sampleToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

  // Set up verifier with appropriate keys
  const verifier = new TokenVerifier(
    { 'default': 'your-256-bit-secret' },
    {
      issuer: 'https://your-trusted-issuer.com',
      audience: 'your-service-id'
    }
  );

  try {
    // Securely verify and decode token
    const [header, payload] = verifier.verifyAndDecode(sampleToken);

    // Only access payload after verification
    console.log(`Verified token for subject: ${payload.sub}`);
  } catch (error) {
    if (error instanceof JWTVerificationError) {
      // Present generic error to user
      console.error(`Authentication failed: ${error.message}`);

      // Log detailed error for debugging (not shown to user)
      console.debug(`Detailed error: ${error.getDetails()}`);
    } else {
      console.error('Unexpected error:', error);
    }
  }
}

// Example invocation
// exampleUsage();