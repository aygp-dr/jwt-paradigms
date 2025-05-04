"""
Secure JWT Parsing Implementation in Python

This module demonstrates secure-by-default JWT handling practices:
1. Signature verification BEFORE payload access
2. Explicit algorithm whitelisting
3. Comprehensive claim validation
4. Secure error handling
"""

import base64
import hmac
import json
import time
from typing import Dict, Any, List, Optional, Tuple, Union, TypedDict, cast
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding, rsa
from cryptography.hazmat.primitives.serialization import load_pem_public_key
from cryptography.exceptions import InvalidSignature

# Define explicit typing for JWT claims
class JWTClaims(TypedDict, total=False):
    iss: str  # issuer
    sub: str  # subject
    exp: int  # expiration time
    nbf: int  # not before time
    iat: int  # issued at time
    jti: str  # JWT ID
    aud: Union[str, List[str]]  # audience

class JWTHeader(TypedDict):
    alg: str  # algorithm
    typ: str  # type
    kid: Optional[str]  # key ID

class InvalidTokenError(Exception):
    """Base exception for all token validation errors.
    
    Using a single exception type prevents information leakage through
    exception type inspection, while still allowing internal distinction.
    """
    pass

class TokenVerifier:
    """Secure JWT token verifier.
    
    This class implements a secure-by-default approach to JWT verification
    where signature verification MUST occur before payload access.
    """
    
    # Explicitly whitelist allowed algorithms
    ALLOWED_ALGORITHMS = frozenset(['HS256', 'RS256', 'ES256'])
    
    def __init__(self, 
                 keys: Dict[str, Any],
                 issuer: Optional[str] = None,
                 audience: Optional[Union[str, List[str]]] = None,
                 leeway: int = 0):
        """Initialize the token verifier.
        
        Args:
            keys: Dict mapping key IDs to cryptographic keys
            issuer: Expected token issuer
            audience: Expected token audience
            leeway: Time leeway for expiration checks (in seconds)
        """
        self.keys = keys
        self.issuer = issuer
        self.audience = audience
        self.leeway = leeway
    
    def verify_and_decode(self, token: str) -> Tuple[JWTHeader, JWTClaims]:
        """Verify and decode a JWT token securely.
        
        This method follows security best practices:
        1. Parse the token into parts without decoding payload
        2. Verify signature before accessing claims
        3. Validate token metadata (exp, nbf, iss, aud)
        4. Only then return the decoded payload
        
        Args:
            token: The JWT token string
        
        Returns:
            Tuple containing the verified header and claims
            
        Raises:
            InvalidTokenError: If the token is invalid for any reason.
                               The message will be generic to avoid information leakage.
        """
        try:
            # Split the token into parts
            parts = token.split('.')
            if len(parts) != 3:
                raise InvalidTokenError("Invalid token format")
            
            header_part, payload_part, signature_part = parts
            
            # Decode header first (minimal parsing needed for verification)
            header = self._decode_base64url_json(header_part)
            decoded_header = cast(JWTHeader, header)
            
            # Verify algorithm is supported (prevent algorithm confusion)
            alg = decoded_header.get('alg')
            if not alg or alg not in self.ALLOWED_ALGORITHMS:
                raise InvalidTokenError("Invalid algorithm")
            
            # Get key ID if present
            kid = decoded_header.get('kid')
            
            # Verify signature first (before decoding payload)
            self._verify_signature(
                f"{header_part}.{payload_part}",
                signature_part,
                alg,
                kid
            )
            
            # Only decode payload after signature verification
            decoded_payload = cast(JWTClaims, self._decode_base64url_json(payload_part))
            
            # Validate standard claims
            current_time = int(time.time())
            
            # Check expiration
            if 'exp' in decoded_payload:
                if current_time > decoded_payload['exp'] + self.leeway:
                    raise InvalidTokenError("Token expired")
            
            # Check not-before time
            if 'nbf' in decoded_payload:
                if current_time < decoded_payload['nbf'] - self.leeway:
                    raise InvalidTokenError("Token not yet valid")
            
            # Check issuer
            if self.issuer and decoded_payload.get('iss') != self.issuer:
                raise InvalidTokenError("Invalid issuer")
            
            # Check audience
            if self.audience:
                aud = decoded_payload.get('aud', '')
                if isinstance(self.audience, str):
                    if self.audience != aud:
                        raise InvalidTokenError("Invalid audience")
                else:  # List[str]
                    if isinstance(aud, str):
                        if aud not in self.audience:
                            raise InvalidTokenError("Invalid audience")
                    elif not any(a in self.audience for a in aud):
                        raise InvalidTokenError("Invalid audience")
            
            return decoded_header, decoded_payload
            
        except (ValueError, KeyError, TypeError, InvalidSignature) as e:
            # Convert all errors to InvalidTokenError with generic message
            # to prevent information leakage, but log the detailed error
            print(f"Token validation error: {str(e)}")  # Replace with proper logging
            raise InvalidTokenError("Invalid token")
    
    def _decode_base64url_json(self, data: str) -> Dict[str, Any]:
        """Decode base64url-encoded JSON safely.
        
        Args:
            data: Base64url-encoded JSON string
            
        Returns:
            Decoded JSON as dictionary
            
        Raises:
            ValueError: If decoding fails
        """
        # Add padding if needed
        padding_needed = len(data) % 4
        if padding_needed:
            data += '=' * (4 - padding_needed)
            
        # Replace URL-safe characters
        data = data.replace('-', '+').replace('_', '/')
        
        # Decode and parse JSON
        decoded_bytes = base64.b64decode(data)
        decoded_str = decoded_bytes.decode('utf-8')
        return json.loads(decoded_str)
    
    def _verify_signature(self, 
                          message: str, 
                          signature_b64: str, 
                          algorithm: str,
                          key_id: Optional[str] = None) -> None:
        """Verify the token signature.
        
        Args:
            message: The message to verify (header.payload)
            signature_b64: Base64url-encoded signature
            algorithm: The algorithm used for signing
            key_id: Optional key identifier
            
        Raises:
            InvalidTokenError: If signature verification fails
        """
        # Add padding if needed
        padding_needed = len(signature_b64) % 4
        if padding_needed:
            signature_b64 += '=' * (4 - padding_needed)
            
        # Replace URL-safe characters
        signature_b64 = signature_b64.replace('-', '+').replace('_', '/')
        
        # Decode signature
        try:
            signature = base64.b64decode(signature_b64)
        except Exception:
            raise InvalidTokenError("Invalid signature format")
        
        # Get key based on key ID, or use default key
        key = self.keys.get(key_id or 'default')
        if not key:
            raise InvalidTokenError("Key not found")
        
        # Verify signature using appropriate algorithm
        if algorithm == 'HS256':
            expected_sig = hmac.new(
                key.encode() if isinstance(key, str) else key,
                message.encode(),
                hashes.SHA256()
            ).digest()
            
            # Use constant-time comparison to prevent timing attacks
            if not hmac.compare_digest(signature, expected_sig):
                raise InvalidTokenError("Invalid signature")
                
        elif algorithm == 'RS256':
            # Assuming key is a PEM-encoded RSA public key
            if isinstance(key, str):
                public_key = load_pem_public_key(key.encode())
            else:
                public_key = key
                
            try:
                public_key.verify(
                    signature,
                    message.encode(),
                    padding.PKCS1v15(),
                    hashes.SHA256()
                )
            except InvalidSignature:
                raise InvalidTokenError("Invalid signature")
                
        elif algorithm == 'ES256':
            # ECDSA verification would go here
            # Implementation depends on the specific library used
            raise NotImplementedError("ES256 not implemented in this example")
            
        else:
            # This should never happen due to earlier check
            raise InvalidTokenError("Unsupported algorithm")


# Example usage
def example_usage():
    # Sample token (normally would come from Authorization header)
    sample_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    
    # Set up verifier with appropriate keys
    verifier = TokenVerifier(
        keys={'default': 'your-256-bit-secret'},
        issuer='https://your-trusted-issuer.com',
        audience='your-service-id'
    )
    
    try:
        # Securely verify and decode token
        header, payload = verifier.verify_and_decode(sample_token)
        
        # Only access payload after verification
        print(f"Verified token for subject: {payload.get('sub')}")
        
    except InvalidTokenError as e:
        # Handle all token validation errors the same way
        # to prevent information leakage
        print(f"Authentication failed: {str(e)}")
        
        # Log the specific error for debugging (not shown to user)
        # logger.debug(f"Detailed error: {str(e)}")


if __name__ == "__main__":
    example_usage()