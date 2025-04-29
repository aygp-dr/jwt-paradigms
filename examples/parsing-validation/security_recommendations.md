# JWT Security Recommendations

## Always Validate Before Using

1. **Verify signatures first** - Never trust token contents before signature verification
2. **Check all required claims** - Validate expiration, issuer, audience, and subject
3. **Validate token type** - Ensure the token is used for its intended purpose
4. **Be mindful of algorithm selection** - Prefer strong algorithms like RS256 over HS256 when possible
5. **Use libraries correctly** - Follow library documentation and security advisories

## Common JWT Attacks to Defend Against

### None Algorithm Attack

The attacker modifies the JWT header to use the "none" algorithm, indicating the token doesn't need a signature:

```json
{
  "alg": "none",
  "typ": "JWT"
}
```

**Defense**: Always validate the algorithm and reject "none" algorithm tokens.

### Algorithm Confusion Attack

The attacker tricks the system into verifying a token signed with one algorithm (like RS256) using another (like HS256):

**Defense**: Always explicitly validate the algorithm in the token matches the expected algorithm.

### Token Injection

The attacker uses a token from one context in another:

**Defense**: Validate the token's audience (`aud`) claim matches your service.

### Replay Attacks

The attacker captures a valid token and reuses it:

**Defense**: Use short expiration times, implement token revocation, and consider using nonces for critical operations.

## Implementation Checklist

- [ ] Use a reputable JWT library
- [ ] Verify signatures before accessing payload
- [ ] Validate all required claims (iss, sub, exp, iat, aud)
- [ ] Implement token revocation for sensitive systems
- [ ] Use secure key management (HSM for production)
- [ ] Apply the principle of least privilege for token permissions
- [ ] Add monitoring for suspicious JWT usage patterns
- [ ] Document token validation requirements for developers

## Code Review Checklist

When reviewing JWT code, look for:

- Signature verification before payload access
- Explicit validation of all relevant claims
- Proper error handling for invalid tokens
- No use of unsafe parsing functions
- Secure key management
- Appropriate token lifetime settings
