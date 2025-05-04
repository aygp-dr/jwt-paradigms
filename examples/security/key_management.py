"""
JWT Key Management Best Practices

This module demonstrates secure key management practices for JWT implementations:
1. Using JWK (JSON Web Keys) for key management
2. Implementing key rotation
3. Securing keys with proper access controls

WARNING: This is an example implementation. In production, use hardware security
modules (HSMs) or secure key management services.
"""

import json
import time
import uuid
from typing import Dict, List, Optional, Any, TypedDict, Union
import jwt
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization


class JWK(TypedDict):
    kid: str  # Key ID
    kty: str  # Key Type (RSA, EC, etc.)
    use: str  # Key Use (sig, enc)
    alg: str  # Algorithm
    n: str    # Modulus (for RSA)
    e: str    # Exponent (for RSA)
    exp: int  # Expiration timestamp
    iat: int  # Issued at timestamp


class KeyStore:
    """Manages a collection of JWKs for token signing and verification."""
    
    def __init__(self, rotation_period: int = 86400):
        """
        Initialize the key store.
        
        Args:
            rotation_period: Key rotation period in seconds (default: 24 hours)
        """
        self.keys: Dict[str, Dict[str, Any]] = {}
        self.current_kid: Optional[str] = None
        self.rotation_period = rotation_period
        
        # Generate initial key
        self._generate_new_key()
    
    def _generate_new_key(self) -> str:
        """
        Generate a new RSA key pair and add it to the key store.
        
        Returns:
            The key ID (kid) of the new key
        """
        # Generate a new RSA key pair
        private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=2048
        )
        
        # Extract public key components
        public_key = private_key.public_key()
        public_numbers = public_key.public_numbers()
        
        # Create a key ID
        kid = str(uuid.uuid4())
        
        # Current time for key metadata
        now = int(time.time())
        
        # Convert private key to PEM format for storage
        private_pem = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        )
        
        # Store the key with metadata
        self.keys[kid] = {
            # Private key data (NEVER expose this)
            "private_key": private_pem,
            
            # Public JWK representation
            "jwk": {
                "kid": kid,
                "kty": "RSA",
                "use": "sig",
                "alg": "RS256",
                "n": self._int_to_base64(public_numbers.n),
                "e": self._int_to_base64(public_numbers.e),
                "iat": now,
                "exp": now + self.rotation_period
            }
        }
        
        # Update current key ID
        self.current_kid = kid
        return kid
    
    def _int_to_base64(self, value: int) -> str:
        """Convert an integer to a Base64URL-encoded string."""
        import base64
        value_hex = format(value, 'x')
        # Ensure even length
        if len(value_hex) % 2 == 1:
            value_hex = '0' + value_hex
        
        value_bytes = bytes.fromhex(value_hex)
        return base64.urlsafe_b64encode(value_bytes).rstrip(b'=').decode('ascii')
    
    def rotate_keys(self) -> str:
        """
        Rotate keys by generating a new key and setting it as the current key.
        Previous keys are retained for token verification until they expire.
        
        Returns:
            The key ID (kid) of the new key
        """
        # Generate a new key
        new_kid = self._generate_new_key()
        
        # Remove expired keys
        now = int(time.time())
        expired_kids = [
            kid for kid, key_data in self.keys.items()
            if key_data["jwk"]["exp"] < now
        ]
        
        for kid in expired_kids:
            del self.keys[kid]
        
        return new_kid
    
    def get_current_private_key(self) -> bytes:
        """Get the current private key for signing tokens."""
        if not self.current_kid or self.current_kid not in self.keys:
            self._generate_new_key()
        
        return self.keys[self.current_kid]["private_key"]
    
    def get_key_by_id(self, kid: str) -> Optional[Dict[str, Any]]:
        """Get a key by its ID."""
        return self.keys.get(kid)
    
    def get_jwks(self) -> Dict[str, List[JWK]]:
        """
        Get the JWKS (JSON Web Key Set) containing all active public keys.
        This endpoint would typically be exposed as /.well-known/jwks.json
        """
        return {
            "keys": [
                key_data["jwk"] for key_data in self.keys.values()
            ]
        }


class JWTManager:
    """Manages JWT token operations with secure key handling."""
    
    def __init__(self, key_store: KeyStore):
        """
        Initialize the JWT manager.
        
        Args:
            key_store: The key store to use for signing and verification
        """
        self.key_store = key_store
    
    def create_token(self, payload: Dict[str, Any], expiry: int = 3600) -> str:
        """
        Create a signed JWT token.
        
        Args:
            payload: The payload to include in the token
            expiry: Token expiration time in seconds (default: 1 hour)
            
        Returns:
            A signed JWT token
        """
        # Check if key rotation is needed
        if (
            not self.key_store.current_kid or
            self.key_store.keys[self.key_store.current_kid]["jwk"]["exp"] < time.time()
        ):
            self.key_store.rotate_keys()
        
        # Get the current key for signing
        current_key = self.key_store.get_current_private_key()
        current_kid = self.key_store.current_kid
        
        # Create a payload with standard claims
        now = int(time.time())
        full_payload = {
            **payload,
            "iat": now,
            "exp": now + expiry,
            "jti": str(uuid.uuid4())  # Unique token ID to prevent replay attacks
        }
        
        # Sign the token with the current private key
        return jwt.encode(
            full_payload,
            current_key,
            algorithm="RS256",
            headers={"kid": current_kid}
        )
    
    def verify_token(self, token: str) -> Dict[str, Any]:
        """
        Verify and decode a JWT token.
        
        Args:
            token: The JWT token to verify
            
        Returns:
            The decoded token payload if valid
            
        Raises:
            jwt.InvalidTokenError: If the token is invalid
        """
        # Get the token header to determine which key to use
        unverified_header = jwt.get_unverified_header(token)
        
        if "kid" not in unverified_header:
            raise jwt.InvalidTokenError("Token missing key ID (kid)")
        
        kid = unverified_header["kid"]
        key_data = self.key_store.get_key_by_id(kid)
        
        if not key_data:
            raise jwt.InvalidTokenError(f"Key ID {kid} not found")
        
        # Verify the token with the appropriate public key
        return jwt.decode(
            token,
            key_data["private_key"],  # In a real implementation, we'd use the public key
            algorithms=["RS256"],
            options={
                "verify_signature": True,
                "require": ["exp", "iat", "jti"]  # Require standard claims
            }
        )


# Example usage
if __name__ == "__main__":
    # Create a key store with a short rotation period for demonstration
    key_store = KeyStore(rotation_period=300)  # 5 minutes
    
    # Create a JWT manager
    jwt_manager = JWTManager(key_store)
    
    # Create a token
    token = jwt_manager.create_token({
        "sub": "user123",
        "name": "John Doe",
        "admin": True,
        "aud": "https://api.example.com"
    })
    
    print(f"Generated token: {token}\n")
    
    # Verify the token
    try:
        payload = jwt_manager.verify_token(token)
        print(f"Token verified successfully: {json.dumps(payload, indent=2)}\n")
    except jwt.InvalidTokenError as e:
        print(f"Token verification failed: {e}")
    
    # Get the JWKS for public consumption
    jwks = key_store.get_jwks()
    print(f"Public JWKS: {json.dumps(jwks, indent=2)}")
    
    print("\nKey rotation demonstration:")
    # Demonstrate key rotation
    old_kid = key_store.current_kid
    new_kid = key_store.rotate_keys()
    print(f"Rotated keys: {old_kid} -> {new_kid}")
    
    # Verify that the old token is still valid after key rotation
    try:
        payload = jwt_manager.verify_token(token)
        print(f"Old token still valid after key rotation: {payload['sub']}")
    except jwt.InvalidTokenError as e:
        print(f"Old token verification failed after key rotation: {e}")