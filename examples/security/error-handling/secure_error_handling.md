# Secure JWT Error Handling

Proper error handling is a critical security aspect of JWT implementations. This document outlines best practices for secure JWT error handling and provides examples in multiple languages.

## Security Risks of Poor Error Handling

1. **Information Leakage**: Detailed error messages can reveal implementation details that help attackers refine their approach.
2. **Fingerprinting**: Different error types for different validation failures allows attackers to enumerate valid user IDs or determine which validations are performed.
3. **Timing Attacks**: Variable processing time based on error type can leak information about valid vs. invalid tokens.
4. **Side-Channel Attacks**: Errors that trigger different system behaviors can be observed externally.

## Core Principles for Secure Error Handling

### 1. Use Consistent, Generic Error Messages

Present a single, consistent error message to clients regardless of the specific validation failure:

```
"Authentication failed. Please log in again."
```

This prevents attackers from distinguishing between different failure modes (invalid signature, expired token, etc.).

### 2. Log Detailed Information Server-Side

While presenting generic messages to clients, log detailed information server-side for debugging and monitoring:

```python
# Python example
try:
    # JWT verification
    verify_token(token)
except InvalidSignatureError as e:
    logger.warning(f"Invalid signature: {e}", extra={
        'token_id': get_token_id(token),
        'user_ip': request.remote_addr
    })
    return jsonify({'error': 'Authentication failed'}), 401
except ExpiredTokenError as e:
    logger.info(f"Expired token: {e}", extra={
        'token_id': get_token_id(token),
        'user_ip': request.remote_addr
    })
    return jsonify({'error': 'Authentication failed'}), 401
```

### 3. Use Constant-Time Comparisons

Prevent timing attacks by using constant-time comparison for signatures and critical values:

```typescript
// TypeScript example
import * as crypto from 'crypto';

function verifySignature(actual: Buffer, expected: Buffer): boolean {
    // Constant-time comparison to prevent timing attacks
    return crypto.timingSafeEqual(actual, expected);
}
```

### 4. Single Exception Type with Internal Differentiation

Use a single exception type externally but maintain detailed information internally:

```java
// Java example
public class JwtSecurityException extends SecurityException {
    private final ErrorCode code;
    private final String detailedMessage;
    
    public JwtSecurityException(ErrorCode code, String detailedMessage) {
        // Public message is always generic
        super("Authentication failed");
        this.code = code;
        this.detailedMessage = detailedMessage;
    }
    
    // For internal logging only
    public ErrorCode getErrorCode() {
        return code;
    }
    
    // For internal logging only
    public String getDetailedMessage() {
        return detailedMessage;
    }
}
```

### 5. Avoid Behavioral Differences Based on Error Type

Ensure the system behaves consistently regardless of the specific JWT validation error:

```python
# Python example - bad practice
if token_expired:
    redirect_to_login()  # Different behavior!
elif invalid_signature:
    return_error_401()
    
# Python example - good practice
if not is_valid_token:
    return_error_401()  # Consistent behavior
```

### 6. Implement Proper Logging Levels

Use appropriate logging levels to distinguish between suspicious activity and normal errors:

- `ERROR`: Security violations that might indicate an attack (signature tampering)
- `WARNING`: Suspicious activity (algorithm confusion attempts)
- `INFO`: Normal operational issues (expired tokens)
- `DEBUG`: Detailed validation information (for development)

### 7. Rate Limit Authentication Failures

Implement rate limiting for authentication failures to prevent brute force attacks:

```typescript
// TypeScript/Express example
import rateLimit from 'express-rate-limit';

const authLimiter = rateLimit({
    windowMs: 15 * 60 * 1000, // 15 minutes
    max: 10, // 10 failed attempts per 15 minutes
    message: 'Too many authentication attempts, please try again later',
    standardHeaders: true,
    legacyHeaders: false,
});

app.use('/api/auth', authLimiter);
```

## Language-Specific Implementation Examples

### Python Implementation

```python
# See secure_error_handling.py for complete implementation
class JWTSecurityError(Exception):
    """Base exception for all JWT security errors.
    
    Uses a generic public message with detailed internal information.
    """
    def __init__(self, error_type: str, detail: str, token_id: Optional[str] = None):
        # Public message is always generic
        super().__init__("Authentication failed")
        # Private attributes for internal logging
        self._error_type = error_type
        self._detail = detail
        self._token_id = token_id
        
    def get_error_type(self) -> str:
        """Get the specific error type for internal logging."""
        return self._error_type
        
    def get_detail(self) -> str:
        """Get detailed error information for internal logging."""
        return self._detail
        
    def get_token_id(self) -> Optional[str]:
        """Get token ID for tracking purposes."""
        return self._token_id
```

### TypeScript Implementation

```typescript
// See secure_error_handling.ts for complete implementation
export class JWTSecurityError extends Error {
    private errorType: string;
    private detail: string;
    private tokenId?: string;
    
    constructor(errorType: string, detail: string, tokenId?: string) {
        // Public message is always generic
        super("Authentication failed");
        this.name = "JWTSecurityError";
        // Private properties for internal logging
        this.errorType = errorType;
        this.detail = detail;
        this.tokenId = tokenId;
    }
    
    // Methods only used internally for logging
    public getErrorType(): string {
        return this.errorType;
    }
    
    public getDetail(): string {
        return this.detail;
    }
    
    public getTokenId(): string | undefined {
        return this.tokenId;
    }
}
```

### Rust Implementation

```rust
// See secure_error_handling.rs for complete implementation
#[derive(Debug)]
pub enum JwtErrorType {
    InvalidSignature,
    Expired,
    NotYetValid,
    InvalidIssuer,
    InvalidAudience,
    InvalidAlgorithm,
    MalformedToken,
    Other,
}

#[derive(Debug)]
pub struct JwtSecurityError {
    // Private fields for internal logging
    error_type: JwtErrorType,
    detail: String,
    token_id: Option<String>,
}

impl JwtSecurityError {
    pub fn new(error_type: JwtErrorType, detail: &str, token_id: Option<String>) -> Self {
        JwtSecurityError {
            error_type,
            detail: detail.to_string(),
            token_id,
        }
    }
    
    // Methods for internal logging
    pub fn error_type(&self) -> &JwtErrorType {
        &self.error_type
    }
    
    pub fn detail(&self) -> &str {
        &self.detail
    }
    
    pub fn token_id(&self) -> Option<&String> {
        self.token_id.as_ref()
    }
}

// Important: Implement Display with generic message
impl std::fmt::Display for JwtSecurityError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Always return generic message to users
        write!(f, "Authentication failed")
    }
}

impl std::error::Error for JwtSecurityError {}
```

## API Response Handling

When returning errors to clients, follow these guidelines:

1. Use appropriate HTTP status codes:
   - `401 Unauthorized`: Authentication failure
   - `403 Forbidden`: Authentication succeeded but permission denied

2. Provide minimal error information:
   ```json
   {
     "error": "Authentication failed",
     "status": 401
   }
   ```

3. Add correlation ID for support without revealing error details:
   ```json
   {
     "error": "Authentication failed",
     "status": 401,
     "correlation_id": "550e8400-e29b-41d4-a716-446655440000"
   }
   ```

## Logging Best Practices

When logging JWT errors:

1. **Include contextual information**:
   - Request ID or correlation ID
   - Source IP (anonymized if needed)
   - Timestamp
   - User agent
   - Endpoint/resource being accessed

2. **Avoid logging sensitive data**:
   - Never log full tokens
   - Mask or hash potentially sensitive claims
   - Be careful with user identifiers

3. **Use structured logging**:
   ```python
   logger.warning("JWT validation failed", extra={
       'error_type': error.get_error_type(),
       'request_id': request_id,
       'source_ip': anonymize_ip(request.remote_addr),
       'resource': request.path
   })
   ```

## Complete Implementation Examples

For detailed implementation examples, see the following files:

- [Python secure error handling](./secure_error_handling.py)
- [TypeScript secure error handling](./secure_error_handling.ts)
- [Rust secure error handling](./secure_error_handling.rs)
- [Java secure error handling](./secure_error_handling.java)

## Testing Error Handling

Test your error handling to ensure it doesn't leak information:

1. **Timing Analysis**: Verify that different error conditions take the same amount of time to process
2. **Response Consistency**: Ensure error responses are identical regardless of error type
3. **Log Verification**: Confirm detailed information is logged correctly while generic errors are returned
4. **Security Scanning**: Use automated tools to test for information leakage

## Conclusion

Secure error handling is essential for JWT security. By using consistent public errors, detailed internal logging, and constant-time operations, you can prevent information leakage while maintaining the ability to debug and monitor your application.