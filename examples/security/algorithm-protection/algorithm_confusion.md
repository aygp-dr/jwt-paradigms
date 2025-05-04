# Preventing Algorithm Confusion Attacks

Algorithm confusion (or algorithm substitution) attacks occur when an attacker manipulates the `alg` header parameter to trick the verification system into using a different algorithm than intended. This document explains these attacks and how to prevent them.

## Common Attack Scenarios

### The "none" Algorithm Attack

In this attack, the attacker changes the algorithm to `"none"`, claiming the token doesn't need signature verification.

**Vulnerable Token:**
```
// Original token signed with HS256
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U

// Modified by attacker to use "none" algorithm
eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJzdWIiOiIxMjM0NTY3ODkwIn0.
```

### RS256 to HS256 Confusion

In this attack, the attacker switches from an asymmetric algorithm (RS256) to a symmetric one (HS256), tricking the system into using the public key (which is public knowledge) as the HMAC secret.

**Vulnerable Scenario:**
1. System expects RS256 (asymmetric) verification with a public key
2. Attacker modifies token to use HS256 (symmetric)
3. Vulnerable systems use the public key as the HMAC secret
4. Attacker can create valid signatures using the public key

## Prevention Techniques

### 1. Explicit Algorithm Whitelisting

Always explicitly specify which algorithms are allowed and reject all others:

```python
# Python example
ALLOWED_ALGORITHMS = frozenset(['HS256', 'RS256'])

if header['alg'] not in ALLOWED_ALGORITHMS:
    raise InvalidTokenError("Algorithm not allowed")
```

```typescript
// TypeScript example
const ALLOWED_ALGORITHMS = Object.freeze(['HS256', 'RS256']);

if (!ALLOWED_ALGORITHMS.includes(header.alg)) {
    throw new Error("Algorithm not allowed");
}
```

### 2. Algorithm-Key Binding

Associate each key with its specific algorithm to prevent confusion:

```python
# Python example
KEY_ALGORITHM_MAP = {
    'key1': 'HS256',
    'key2': 'RS256',
}

if header['alg'] != KEY_ALGORITHM_MAP[key_id]:
    raise InvalidTokenError("Algorithm mismatch for key")
```

### 3. Type-Safe Key Selection

Use different types for different key algorithms to make confusion impossible:

```typescript
// TypeScript example
interface SymmetricKey {
    type: 'symmetric';
    value: string;
    algorithm: 'HS256' | 'HS384' | 'HS512';
}

interface AsymmetricKey {
    type: 'asymmetric';
    publicKey: string;
    algorithm: 'RS256' | 'ES256';
}

type Key = SymmetricKey | AsymmetricKey;

// Type-safe key selection
function getKey(keyId: string, algorithm: string): Key {
    const key = keys.get(keyId);
    if (!key) throw new Error("Key not found");
    
    if (key.algorithm !== algorithm) {
        throw new Error("Algorithm mismatch");
    }
    
    return key;
}
```

### 4. Reject "none" Algorithm

Always explicitly reject the "none" algorithm:

```java
// Java example
if ("none".equalsIgnoreCase(algorithm)) {
    throw new JwtException("Algorithm 'none' is not allowed");
}
```

### 5. Use Appropriate Key Types

Ensure your verification system uses the correct key type for each algorithm:

```python
# Python example
def verify_signature(algorithm, message, signature, key):
    if algorithm.startswith('HS'):
        if not isinstance(key, bytes):
            raise TypeError("HMAC requires bytes key")
        # HMAC verification
    elif algorithm.startswith('RS'):
        if not isinstance(key, RSAPublicKey):
            raise TypeError("RSA requires public key")
        # RSA verification
```

## Real-World Example: JWT Library Security

Many JWT libraries have had vulnerabilities related to algorithm confusion. When selecting a library, verify it has:

1. Default algorithm enforcement (no implicit "any algorithm" acceptance)
2. Explicit key typing to prevent using the wrong key type
3. No support for the "none" algorithm, or explicit blocking of it
4. Strong typing for algorithm selection

## Language-Specific Examples

See the implementation files in this directory for complete, secure examples:

- [Python Implementation](./python_alg_protection.py)
- [TypeScript Implementation](./typescript_alg_protection.ts)
- [Java Implementation](./java_alg_protection.java)
- [Rust Implementation](./rust_alg_protection.rs)