import base64
import json
from typing import Dict, Any, TypedDict, Optional, Union

class JwtHeader(TypedDict):
    alg: str
    typ: str

class JwtParseResult(TypedDict):
    header: JwtHeader
    is_verified: bool

def decode_base64url(segment: str) -> bytes:
    """
    Decode a base64url-encoded string to bytes.
    Handles URL-safe character conversion and padding.
    """
    # Add padding if needed
    padding_needed = len(segment) % 4
    if padding_needed:
        segment += '=' * (4 - padding_needed)
    
    # Convert from URL-safe base64 to standard base64
    segment = segment.replace('-', '+').replace('_', '/')
    
    # Decode
    return base64.b64decode(segment)

def decode_jwt_header(auth_header: str) -> JwtHeader:
    """
    WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
    NEVER use this approach for authentication or authorization in production.
    ALWAYS verify the JWT signature before trusting any data in the token.
    """
    try:
        token = auth_header.split(' ')[1]
        header_part = token.split('.')[0]
        
        # Decode base64url
        decoded_bytes = decode_base64url(header_part)
        decoded_str = decoded_bytes.decode('utf-8')
        
        # Parse JSON
        header = json.loads(decoded_str)
        
        # Validate structure
        if not isinstance(header, dict):
            raise ValueError("JWT header is not a JSON object")
        
        # Verify required fields
        if 'alg' not in header:
            raise ValueError("JWT header missing 'alg' field")
        if 'typ' not in header:
            raise ValueError("JWT header missing 'typ' field")
            
        # Return properly typed header
        return header
    except Exception as e:
        raise ValueError(f"Invalid JWT header: {str(e)}")

# Example usage
auth_header = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"
print(decode_jwt_header(auth_header))

"""
SECURE ALTERNATIVE:
import jwt
from typing import Dict, Any, TypedDict, Optional, Union

class JwtVerifyResult(TypedDict, total=False):
    valid: bool
    header: Optional[Dict[str, Any]]
    payload: Optional[Dict[str, Any]]
    error: Optional[str]

def securely_verify_jwt(token: str, secret_key: str) -> JwtVerifyResult:
    try:
        # VERIFY FIRST - this validates the signature
        payload = jwt.decode(
            token, 
            secret_key, 
            algorithms=['HS256'],  # Explicitly specify allowed algorithms
            options={"verify_signature": True}
        )
        
        # Now we can safely get the header
        header = jwt.get_unverified_header(token)
        
        return {
            "valid": True,
            "header": header,
            "payload": payload
        }
    except jwt.InvalidTokenError as e:
        return {
            "valid": False,
            "error": str(e),
            "header": None,
            "payload": None
        }
"""
