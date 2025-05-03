# Defense-in-Depth for JWT Implementations

This document outlines a comprehensive defense-in-depth strategy for JWT implementations, with multiple layers of security controls.

## 1. Architectural Controls

### 1.1 Segmentation
- Place JWT issuance behind dedicated authentication services
- Implement token exchange services for cross-domain authentication
- Use separate signing keys for different security domains

### 1.2 Token Scope Limitation
- Issue tokens with minimum required permissions
- Use different token types for different purposes (access, refresh, ID)
- Consider using short-lived tokens for sensitive operations

## 2. Implementation Controls

### 2.1 Secure Token Creation
- Use cryptographically strong algorithms (RS256/ES256 over HS256)
- Include all necessary claims (iss, sub, aud, exp, iat, jti)
- Sign tokens with appropriate key length (RSA ≥2048 bits, EC ≥P-256)

```python
# Example of secure token creation
def create_secure_token(user_id, permissions, audience):
    payload = {
        'sub': user_id,
        'iss': 'https://auth.example.com',
        'aud': audience,
        'iat': int(time.time()),
        'exp': int(time.time()) + 3600,  # 1 hour expiration
        'jti': str(uuid.uuid4()),
        'permissions': permissions
    }
    
    # Use RS256 with a 2048-bit key
    return jwt.encode(payload, PRIVATE_KEY, algorithm='RS256')
```

### 2.2 Secure Token Validation

```typescript
// Example validation pipeline
function validateToken(token: string): ValidationResult {
  try {
    // 1. Verify signature first
    const payload = jwt.verify(token, PUBLIC_KEY, {
      algorithms: ['RS256'],      // Only allow RS256
      audience: 'api.example.com', // Verify audience
      issuer: 'auth.example.com',  // Verify issuer
      clockTolerance: 30          // Allow 30s clock skew
    });
    
    // 2. Additional validations after signature verification
    if (!payload.jti) throw new Error('Missing token ID');
    if (!Array.isArray(payload.permissions)) throw new Error('Invalid permissions');
    
    // 3. Check blacklist
    if (isTokenBlacklisted(payload.jti)) throw new Error('Token revoked');
    
    return { valid: true, payload };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}
```

## 3. Runtime Controls

### 3.1 Token Revocation
- Implement token blacklisting/revocation 
- Use Redis/other cache for blacklist with TTL matching token expiration
- Force token refresh on security events (password change, suspicious activity)

### 3.2 Monitoring and Detection
- Log JWT validation failures with appropriate context
- Monitor for suspicious patterns (same token from different IPs, high rate of failures)
- Implement rate limiting for authentication endpoints
- Track failed validation reason codes

```typescript
// Example monitoring logic
function monitorTokenUsage(token, request, validationResult) {
  // Never log the actual token, use a hash instead
  const tokenHash = createHash('sha256').update(token).digest('hex').substring(0, 16);
  
  logger.info('Token validation', {
    tokenHash,
    ip: request.ip,
    success: validationResult.valid,
    errorCode: validationResult.error ? validationResult.error.code : null,
    userId: validationResult.valid ? validationResult.payload.sub : null,
    userAgent: request.headers['user-agent']
  });
  
  // Alert on suspicious patterns
  if (suspiciousPattern(request, validationResult)) {
    alertSecurityTeam({
      event: 'suspicious_jwt_usage',
      details: { /* relevant details */ }
    });
  }
}
```

## 4. Key Management Controls

### 4.1 Key Rotation
- Rotate signing keys regularly (e.g., every 90 days)
- Implement overlapping validity periods for smooth transitions
- Use a secure key management service or HSM in production

### 4.2 JWK Endpoint Security
- Expose public keys via a `/.well-known/jwks.json` endpoint
- Set appropriate Cache-Control headers
- Implement HTTPS with strong TLS configuration
- Consider access controls for private JWK endpoints

## 5. Additional Protection Mechanisms

### 5.1 Sender Constraints
- Bind tokens to specific origins using `cnf` claim (RFC 7800)
- Consider implementing proof-of-possession tokens
- Add fingerprinting information for high-value tokens

### 5.2 Secure Transport
- Always use HTTPS for token transmission
- Set appropriate security headers (Strict-Transport-Security, etc.)
- Use secure and HttpOnly cookie flags for web applications
- Consider encrypted JWTs (JWE) for highly sensitive data

## 6. Layered Validation Example

The following example demonstrates multiple validation layers for defense-in-depth:

```python
def validate_with_defense_in_depth(token, request):
    # Layer 1: Basic structure checks
    if not token or not isinstance(token, str):
        return fail('Invalid token format')
    
    # Layer 2: Rate limiting
    if is_rate_limited(request.ip):
        return fail('Rate limit exceeded')
    
    try:
        # Layer 3: Signature verification (critical)
        payload = jwt.decode(
            token, 
            get_public_key(),
            algorithms=['RS256'],
            audience='api.example.com',
            issuer='auth.example.com'
        )
        
        # Layer 4: Additional claim validation
        validate_claims(payload)
        
        # Layer 5: Token freshness checks
        if is_token_too_old(payload.iat):
            return fail('Token too old')
        
        # Layer 6: Revocation check
        if is_token_revoked(payload.jti):
            return fail('Token revoked')
        
        # Layer 7: Permission checks for the specific operation
        if not has_permission(payload, request.operation):
            return fail('Insufficient permissions')
        
        # Layer 8: Anomaly detection
        record_token_usage(payload, request)
        if is_anomalous_usage(payload, request):
            alert_security_team(payload, request)
            return fail('Suspicious activity detected')
        
        # All checks passed
        return success(payload)
    
    except jwt.InvalidTokenError as e:
        log_validation_failure(token_hash(token), str(e), request)
        return fail('Invalid token')
    except Exception as e:
        log_unexpected_error(str(e), request)
        return fail('Authentication error')
```

## 7. JWT Attack Mitigations

| Attack | Description | Mitigation |
|--------|-------------|------------|
| None Algorithm | Setting `alg: "none"` to bypass verification | Explicitly specify allowed algorithms |
| Algorithm Confusion | Using RS256 header with HS256 validation | Validate algorithm before verification |
| Directory Traversal | Crafting a `kid` header with `../` | Validate and sanitize all header fields |
| Key ID SQL Injection | Injecting SQL into `kid` header | Parameterize queries for key lookup |
| Weak Signature | Using weak keys for signing | Enforce minimum key strength |
| Embedded Script | Including JS or HTML in token claims | Sanitize outputs when displaying token data |
| Token Sidejacking | Stealing and reusing tokens | Bind tokens to client properties |

## 8. Additional Resources

- [IETF JWT Best Practices](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-jwt-bcp)
- [OWASP JWT Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/JSON_Web_Token_for_Java_Cheat_Sheet.html)
- [JWT Security (auth0)](https://auth0.com/docs/secure/tokens/json-web-tokens/jwt-security-best-practices)
- [JWT.io](https://jwt.io/)
- [RFC 7519: JSON Web Token](https://tools.ietf.org/html/rfc7519)