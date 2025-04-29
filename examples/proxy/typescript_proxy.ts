import * as jwt from 'jsonwebtoken';
import * as crypto from 'crypto';
import { Request, Response, NextFunction } from 'express';

// Configuration
const SECRET_KEY = process.env.JWT_SECRET || 'your-secret-key';
const SERVICE_ID = 'analytics-service';
const TTL_SECONDS = 300; // 5 minutes

interface ProxyTokenPayload {
  sub: string;        // Original user ID
  iss: string;        // Issuing service
  aud: string;        // Target service
  orig: string;       // Original token fingerprint
  iat: number;        // Issued at time
  exp: number;        // Expiration time
  scope: string[];    // Allowed actions
  ctx: {              // Context data
    req_id: string;
    source_ip: string;
    [key: string]: any;
  };
}

/**
 * Generate a proxy token for making downstream service calls
 */
function generateProxyToken(
  userId: string,
  originalToken: string,
  targetService: string,
  scopes: string[],
  requestContext: Record<string, any>
): string {
  // Create fingerprint of original token to prevent token substitution attacks
  const tokenFingerprint = crypto
    .createHash('sha256')
    .update(originalToken)
    .digest('hex');
  
  const now = Math.floor(Date.now() / 1000);
  
  const payload: ProxyTokenPayload = {
    sub: userId,
    iss: SERVICE_ID,
    aud: targetService,
    orig: tokenFingerprint,
    iat: now,
    exp: now + TTL_SECONDS,
    scope: scopes,
    ctx: {
      req_id: requestContext.requestId || crypto.randomUUID(),
      source_ip: requestContext.ip || '0.0.0.0',
      ...requestContext
    }
  };
  
  return jwt.sign(payload, SECRET_KEY, { algorithm: 'HS256' });
}

/**
 * Verify a proxy token in a downstream service
 */
function verifyProxyToken(token: string, expectedAudience: string): ProxyTokenPayload | null {
  try {
    const decoded = jwt.verify(token, SECRET_KEY, {
      algorithms: ['HS256'],
      audience: expectedAudience
    }) as ProxyTokenPayload;
    
    return decoded;
  } catch (error) {
    console.error('Proxy token verification failed:', error);
    return null;
  }
}

/**
 * Express middleware for proxy authentication
 */
function proxyAuthMiddleware(req: Request, res: Response, next: NextFunction): void {
  const proxyToken = req.headers.authorization?.replace('Bearer ', '');
  
  if (!proxyToken) {
    res.status(401).json({ error: 'Proxy authentication required' });
    return;
  }
  
  const serviceId = req.headers['x-service-id'] as string;
  
  if (!serviceId) {
    res.status(400).json({ error: 'Service ID header required' });
    return;
  }
  
  const decoded = verifyProxyToken(proxyToken, SERVICE_ID);
  
  if (!decoded) {
    res.status(403).json({ error: 'Invalid proxy token' });
    return;
  }
  
  // Check if the calling service is the expected issuer
  if (decoded.iss !== serviceId) {
    res.status(403).json({ error: 'Token issuer mismatch' });
    return;
  }
  
  // Add the proxy context to the request for later use
  req['proxyContext'] = {
    userId: decoded.sub,
    scopes: decoded.scope,
    context: decoded.ctx
  };
  
  next();
}

// Example usage
const originalUserToken = 'eyJhbGciOiJIUzI1NiIsInR...';
const userId = 'user456';
const targetService = 'payment-service';
const allowedScopes = ['read:transactions', 'process:refund'];
const requestContext = {
  requestId: 'req_12345',
  ip: '192.168.1.100',
  userAgent: 'Mozilla/5.0...',
  route: '/api/transactions'
};

const proxyToken = generateProxyToken(
  userId,
  originalUserToken,
  targetService,
  allowedScopes,
  requestContext
);

console.log('Proxy Token:', proxyToken);

// Simulate verification in the target service
const verificationResult = verifyProxyToken(proxyToken, targetService);
console.log('Verification Result:', verificationResult);
