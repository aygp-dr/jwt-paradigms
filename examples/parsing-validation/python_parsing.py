import base64
import json
from typing import Dict, Any, Optional, List, Union, TypedDict
from datetime import datetime, timezone
import hmac
import hashlib

# Define proper types for JWT structures
class JWTHeader(TypedDict):
    alg: str
    typ: str
    kid: Optional[str]

class JWTClaims(TypedDict, total=False):
    # Required claims
    iss: str        # issuer
    sub: str        # subject
    exp: int        # expiration time
    iat: int        # issued at time
    # Optional claims
    aud: Optional[Union[str, List[str]]]  # audience
    nbf: Optional[int]  # not before time
    jti: Optional[str]  # JWT ID

class ParsedJWT(TypedDict):
    header: JWTHeader
    payload: JWTClaims
    signature_segment: str
    is_verified: bool

def constant_time_compare(val1: bytes, val2: bytes) -> bool:
    """
    Compare two byte strings in constant time to prevent timing attacks.
    """
    if len(val1) != len(val2):
        return False
    
    result = 0
    for x, y in zip(val1, val2):
        result |= x ^ y
    return result == 0

def parse_jwt_unsafe(token: str) -> ParsedJWT:
    """
    UNSAFE: Parse a JWT without signature verification
    Only use for debugging or examining token structure
    NEVER use for authentication decisions
    """
    # Split the token into parts
    parts = token.split('.')
    if len(parts) != 3:
        raise ValueError("Invalid JWT format")
    
    # Base64url decode the header and payload
    def decode_segment(segment):
        # Add padding if needed
        padding = len(segment) % 4
        if padding:
            segment += '=' * (4 - padding)
        
        # Convert from URL-safe base64 to standard base64
        segment = segment.replace('-', '+').replace('_', '/')
        
        # Decode
        decoded_bytes = base64.b64decode(segment)
        return json.loads(decoded_bytes.decode('utf-8'))
    
    # Parse header and payload
    header = decode_segment(parts[0])
    payload = decode_segment(parts[1])
    
    # Return parsed data (note: signature is NOT verified)
    return {
        'header': header,
        'payload': payload,
        'signature_segment': parts[2],  # Raw, still encoded
        'is_verified': False  # Explicitly mark as unverified
    }

def verify_and_parse_jwt(token: str, secret_key: str, 
                         allowed_algorithms: List[str] = ['HS256']) -> ParsedJWT:
    """
    Securely verify and parse a JWT token.
    
    Args:
        token: The JWT token string
        secret_key: The key used to verify the signature
        allowed_algorithms: List of allowed algorithms (prevents alg downgrade attacks)
    
    Returns:
        A dictionary with the verified header, payload, and signature information
        
    Raises:
        ValueError: For any validation errors including expired tokens, 
                   invalid signatures, and disallowed algorithms
    """
    # Split the token into parts
    parts = token.split('.')
    if len(parts) != 3:
        raise ValueError("Invalid JWT format")
    
    # 1. Parse the header to determine the algorithm
    header_part = parts[0]
    
    # Add padding if needed
    padding = len(header_part) % 4
    if padding:
        padded_header = header_part + '=' * (4 - padding)
    else:
        padded_header = header_part
    
    # Convert from URL-safe base64 to standard base64
    padded_header = padded_header.replace('-', '+').replace('_', '/')
    
    # Decode header
    try:
        header_bytes = base64.b64decode(padded_header)
        header = json.loads(header_bytes.decode('utf-8'))
    except Exception:
        raise ValueError("Invalid token header")
    
    # 2. Verify algorithm is allowed (prevents 'none' attack and algorithm confusion)
    if 'alg' not in header:
        raise ValueError("Algorithm not specified in token")
    
    alg = header['alg']
    if alg not in allowed_algorithms:
        raise ValueError(f"Algorithm {alg} not allowed")
    
    # 3. Verify signature BEFORE decoding payload
    if alg == 'HS256':
        # Compute expected signature
        message = f"{parts[0]}.{parts[1]}".encode()
        expected_signature = hmac.new(
            secret_key.encode(), 
            message, 
            hashlib.sha256
        ).digest()
        
        # Decode actual signature
        signature_part = parts[2]
        # Add padding if needed
        padding = len(signature_part) % 4
        if padding:
            signature_part += '=' * (4 - padding)
        signature_part = signature_part.replace('-', '+').replace('_', '/')
        try:
            actual_signature = base64.b64decode(signature_part)
        except Exception:
            raise ValueError("Invalid signature encoding")
        
        # Verify signature using constant time comparison
        if not constant_time_compare(expected_signature, actual_signature):
            raise ValueError("Invalid signature")
    else:
        # For non-HMAC algorithms, you would use appropriate verification
        # libraries like cryptography or PyJWT
        raise ValueError(f"Algorithm {alg} verification not implemented")
    
    # 4. Only after signature verification, decode payload
    payload_part = parts[1]
    # Add padding if needed
    padding = len(payload_part) % 4
    if padding:
        payload_part += '=' * (4 - padding)
    payload_part = payload_part.replace('-', '+').replace('_', '/')
    
    try:
        payload_bytes = base64.b64decode(payload_part)
        payload = json.loads(payload_bytes.decode('utf-8'))
    except Exception:
        raise ValueError("Invalid token payload")
    
    # 5. Validate claims
    current_time = int(datetime.now(timezone.utc).timestamp())
    
    # Check expiration
    if 'exp' in payload and current_time > payload['exp']:
        raise ValueError("Token has expired")
    
    # Check not before time
    if 'nbf' in payload and current_time < payload['nbf']:
        raise ValueError("Token not yet valid")
    
    # Return verified data
    return {
        'header': header,
        'payload': payload,
        'signature_segment': parts[2],
        'is_verified': True
    }

# Example usage
if __name__ == "__main__":
    # Sample token (DO NOT use real tokens in code)
    token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    
    print("\n===== INSECURE PARSING (DEMO ONLY) =====")
    try:
        # INSECURE - only for demonstration!
        parsed = parse_jwt_unsafe(token)
        print("⚠️ WARNING: This is unsafe parsing and should NEVER be used for authentication!")
        print(f"Header: {parsed['header']}")
        print(f"Payload: {parsed['payload']}")
    except Exception as e:
        print(f"Error parsing token: {e}")
    
    print("\n===== SECURE PARSING WITH VERIFICATION =====")
    try:
        # The secret used for this sample token is "your-256-bit-secret"
        verified = verify_and_parse_jwt(token, "your-256-bit-secret")
        print("✅ Token successfully verified!")
        print(f"Header: {verified['header']}")
        print(f"Payload: {verified['payload']}")
        
        # Demonstrate verification failure with wrong key
        print("\n===== VERIFICATION WITH WRONG KEY =====")
        verify_and_parse_jwt(token, "wrong-secret")
    except ValueError as e:
        print(f"Verification error: {e}")
