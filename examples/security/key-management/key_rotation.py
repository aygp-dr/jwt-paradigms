"""
JWT Key Management and Rotation Example

This module demonstrates secure key management practices for JWT:
1. JWK (JSON Web Key) handling
2. Key rotation with overlapping validity periods
3. Key ID (kid) selection for verification
4. Secure key storage patterns
"""

import json
import time
import uuid
from typing import Dict, List, Optional, Any, Tuple, cast
from datetime import datetime, timedelta
import base64
import hashlib
import hmac
import os
import secrets

# For RSA key handling
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.backends import default_backend


class JWK:
    """JSON Web Key implementation with rotation support."""
    
    def __init__(self, 
                 kid: str,
                 kty: str,
                 alg: str,
                 key_data: Dict[str, Any],
                 not_before: Optional[datetime] = None,
                 expiry: Optional[datetime] = None):
        """Initialize a JSON Web Key (JWK).
        
        Args:
            kid: Key ID
            kty: Key type (RSA, EC, oct)
            alg: Algorithm (RS256, ES256, HS256)
            key_data: Key material (format depends on kty)
            not_before: When the key becomes valid
            expiry: When the key expires
        """
        self.kid = kid
        self.kty = kty
        self.alg = alg
        self.key_data = key_data
        self.not_before = not_before or datetime.now()
        self.expiry = expiry
    
    def is_valid(self) -> bool:
        """Check if the key is currently valid based on time bounds."""
        now = datetime.now()
        if now < self.not_before:
            return False
        if self.expiry and now > self.expiry:
            return False
        return True
    
    def to_jwk_dict(self, include_private: bool = False) -> Dict[str, Any]:
        """Convert to JWK format dictionary.
        
        Args:
            include_private: Whether to include private key material
            
        Returns:
            JWK as a dictionary
        """
        jwk = {
            "kid": self.kid,
            "kty": self.kty,
            "alg": self.alg,
            "use": "sig",  # Signature key
            # Include key-specific fields from key_data
            **{k: v for k, v in self.key_data.items() 
               if include_private or not k.startswith("d")},
        }
        
        # Add key usage timeframe
        if self.not_before:
            jwk["nbf"] = int(self.not_before.timestamp())
        if self.expiry:
            jwk["exp"] = int(self.expiry.timestamp())
            
        return jwk
    
    @classmethod
    def from_jwk_dict(cls, jwk_dict: Dict[str, Any]) -> 'JWK':
        """Create a JWK from a JWK format dictionary.
        
        Args:
            jwk_dict: JWK as a dictionary
            
        Returns:
            JWK instance
        """
        # Extract standard fields
        kid = jwk_dict.get("kid", str(uuid.uuid4()))
        kty = jwk_dict["kty"]
        alg = jwk_dict["alg"]
        
        # Extract time bounds
        not_before = None
        if "nbf" in jwk_dict:
            not_before = datetime.fromtimestamp(jwk_dict["nbf"])
            
        expiry = None
        if "exp" in jwk_dict:
            expiry = datetime.fromtimestamp(jwk_dict["exp"])
        
        # Extract key data (removing standard fields)
        key_data = {k: v for k, v in jwk_dict.items() 
                   if k not in ["kid", "kty", "alg", "use", "nbf", "exp"]}
        
        return cls(kid, kty, alg, key_data, not_before, expiry)
    
    @classmethod
    def generate_rsa_key(cls, 
                        kid: Optional[str] = None,
                        key_size: int = 2048,
                        alg: str = "RS256",
                        valid_for: Optional[timedelta] = None) -> 'JWK':
        """Generate a new RSA key pair.
        
        Args:
            kid: Key ID (random UUID if not provided)
            key_size: RSA key size in bits
            alg: Algorithm (default: RS256)
            valid_for: Key validity period (default: 90 days)
            
        Returns:
            JWK instance with RSA key pair
        """
        # Generate RSA key
        private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=key_size,
            backend=default_backend()
        )
        
        # Get public key in JWK format
        public_numbers = private_key.public_key().public_numbers()
        
        # Prepare key data in JWK format
        key_data = {
            "n": base64.urlsafe_b64encode(public_numbers.n.to_bytes(
                (public_numbers.n.bit_length() + 7) // 8, byteorder='big'
            )).decode('utf-8').rstrip("="),
            "e": base64.urlsafe_b64encode(public_numbers.e.to_bytes(
                (public_numbers.e.bit_length() + 7) // 8, byteorder='big'
            )).decode('utf-8').rstrip("="),
        }
        
        # Add private key components
        private_numbers = private_key.private_numbers()
        key_data["d"] = base64.urlsafe_b64encode(private_numbers.d.to_bytes(
            (private_numbers.d.bit_length() + 7) // 8, byteorder='big'
        )).decode('utf-8').rstrip("=")
        
        # Add prime factors and other private components
        key_data["p"] = base64.urlsafe_b64encode(private_numbers.p.to_bytes(
            (private_numbers.p.bit_length() + 7) // 8, byteorder='big'
        )).decode('utf-8').rstrip("=")
        key_data["q"] = base64.urlsafe_b64encode(private_numbers.q.to_bytes(
            (private_numbers.q.bit_length() + 7) // 8, byteorder='big'
        )).decode('utf-8').rstrip("=")
        
        # Set key ID and validity period
        kid = kid or str(uuid.uuid4())
        not_before = datetime.now()
        expiry = not_before + (valid_for or timedelta(days=90))
        
        return cls(kid, "RSA", alg, key_data, not_before, expiry)
    
    @classmethod
    def generate_hmac_key(cls,
                         kid: Optional[str] = None,
                         key_size: int = 256,
                         alg: str = "HS256",
                         valid_for: Optional[timedelta] = None) -> 'JWK':
        """Generate a new HMAC key.
        
        Args:
            kid: Key ID (random UUID if not provided)
            key_size: Key size in bits
            alg: Algorithm (default: HS256)
            valid_for: Key validity period (default: 90 days)
            
        Returns:
            JWK instance with HMAC key
        """
        # Generate random key with appropriate length
        key_bytes = secrets.token_bytes(key_size // 8)
        
        # Prepare key data in JWK format
        key_data = {
            "k": base64.urlsafe_b64encode(key_bytes).decode('utf-8').rstrip("=")
        }
        
        # Set key ID and validity period
        kid = kid or str(uuid.uuid4())
        not_before = datetime.now()
        expiry = not_before + (valid_for or timedelta(days=90))
        
        return cls(kid, "oct", alg, key_data, not_before, expiry)
    
    def get_cryptographic_key(self) -> Any:
        """Convert JWK to a cryptographic key object.
        
        Returns:
            A key object appropriate for the algorithm
        """
        if self.kty == "RSA":
            if self.alg.startswith("RS"):
                # Check if we have private key material
                if "d" in self.key_data:
                    # Convert JWK to RSA private key
                    n = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["n"] + "=="), 
                        byteorder='big'
                    )
                    e = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["e"] + "=="), 
                        byteorder='big'
                    )
                    d = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["d"] + "=="), 
                        byteorder='big'
                    )
                    p = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["p"] + "=="), 
                        byteorder='big'
                    )
                    q = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["q"] + "=="), 
                        byteorder='big'
                    )
                    
                    # Calculate other CRT parameters
                    dp = d % (p - 1)
                    dq = d % (q - 1)
                    qi = pow(q, -1, p)
                    
                    # Create private key
                    private_numbers = rsa.RSAPrivateNumbers(
                        p=p, q=q, d=d, dmp1=dp, dmq1=dq, iqmp=qi,
                        public_numbers=rsa.RSAPublicNumbers(e=e, n=n)
                    )
                    return private_numbers.private_key(default_backend())
                else:
                    # Convert JWK to RSA public key
                    n = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["n"] + "=="), 
                        byteorder='big'
                    )
                    e = int.from_bytes(
                        base64.urlsafe_b64decode(self.key_data["e"] + "=="), 
                        byteorder='big'
                    )
                    public_numbers = rsa.RSAPublicNumbers(e=e, n=n)
                    return public_numbers.public_key(default_backend())
        
        elif self.kty == "oct":
            if self.alg.startswith("HS"):
                # Convert JWK to HMAC key
                return base64.urlsafe_b64decode(self.key_data["k"] + "==")
        
        # Not implemented for other key types
        raise NotImplementedError(f"Key type {self.kty} with algorithm {self.alg} not implemented")


class KeyManager:
    """Key manager for JWT with rotation support."""
    
    def __init__(self, jwks_path: Optional[str] = None):
        """Initialize key manager.
        
        Args:
            jwks_path: Path to JWKS file (JSON Web Key Set)
        """
        self.keys: Dict[str, JWK] = {}
        self.primary_signing_key: Optional[str] = None
        self.jwks_path = jwks_path
        
        # Load keys if path provided
        if jwks_path and os.path.exists(jwks_path):
            self.load_keys()
    
    def load_keys(self) -> None:
        """Load keys from JWKS file."""
        if not self.jwks_path:
            return
        
        try:
            with open(self.jwks_path, 'r') as f:
                jwks_data = json.load(f)
                
            if 'keys' in jwks_data:
                for jwk_dict in jwks_data['keys']:
                    jwk = JWK.from_jwk_dict(jwk_dict)
                    self.keys[jwk.kid] = jwk
                    
            if 'primary' in jwks_data:
                self.primary_signing_key = jwks_data['primary']
        except (json.JSONDecodeError, IOError) as e:
            print(f"Error loading JWKS file: {e}")
    
    def save_keys(self) -> None:
        """Save keys to JWKS file."""
        if not self.jwks_path:
            return
        
        jwks_data = {
            'keys': [
                k.to_jwk_dict(include_private=True) for k in self.keys.values()
            ],
        }
        
        if self.primary_signing_key:
            jwks_data['primary'] = self.primary_signing_key
        
        try:
            # Create directory if it doesn't exist
            os.makedirs(os.path.dirname(self.jwks_path), exist_ok=True)
            
            # Write to a temporary file and rename for atomic write
            temp_path = f"{self.jwks_path}.tmp"
            with open(temp_path, 'w') as f:
                json.dump(jwks_data, f, indent=2)
            
            os.replace(temp_path, self.jwks_path)
        except IOError as e:
            print(f"Error saving JWKS file: {e}")
    
    def get_jwks_public(self) -> Dict[str, Any]:
        """Get JWKS with only public keys.
        
        Returns:
            JWKS dictionary with public keys only
        """
        return {
            'keys': [
                k.to_jwk_dict(include_private=False) 
                for k in self.keys.values() 
                if k.is_valid()
            ]
        }
    
    def add_key(self, jwk: JWK) -> None:
        """Add a key to the manager.
        
        Args:
            jwk: The key to add
        """
        self.keys[jwk.kid] = jwk
        
        # If no primary key set, use this one
        if not self.primary_signing_key:
            self.primary_signing_key = jwk.kid
            
        # Save changes
        self.save_keys()
    
    def remove_key(self, kid: str) -> None:
        """Remove a key from the manager.
        
        Args:
            kid: Key ID to remove
        """
        if kid in self.keys:
            del self.keys[kid]
            
            # If this was the primary key, clear it
            if self.primary_signing_key == kid:
                self.primary_signing_key = None
                
                # Find a new primary key if possible
                valid_keys = [k for k in self.keys.values() if k.is_valid()]
                if valid_keys:
                    self.primary_signing_key = valid_keys[0].kid
            
            # Save changes
            self.save_keys()
    
    def get_key(self, kid: str) -> Optional[JWK]:
        """Get a key by ID.
        
        Args:
            kid: Key ID to retrieve
            
        Returns:
            The key if found and valid, None otherwise
        """
        key = self.keys.get(kid)
        if key and key.is_valid():
            return key
        return None
    
    def get_signing_key(self) -> Optional[JWK]:
        """Get the current primary signing key.
        
        Returns:
            The primary signing key if set and valid, None otherwise
        """
        if not self.primary_signing_key:
            return None
            
        key = self.keys.get(self.primary_signing_key)
        if key and key.is_valid():
            return key
            
        # Primary key not valid, try to find a new one
        valid_keys = [k for k in self.keys.values() if k.is_valid()]
        if valid_keys:
            self.primary_signing_key = valid_keys[0].kid
            self.save_keys()
            return valid_keys[0]
            
        return None
    
    def rotate_keys(self, 
                   key_type: str = "RSA", 
                   algorithm: str = "RS256",
                   overlap_days: int = 2) -> None:
        """Rotate keys by generating a new primary key.
        
        This implements key rotation with an overlap period where both
        the old and new keys are valid. This ensures tokens signed with
        the old key remain valid during the transition.
        
        Args:
            key_type: Key type to generate ("RSA" or "oct")
            algorithm: Algorithm to use
            overlap_days: Number of days to keep old key valid
        """
        # Generate new key
        if key_type == "RSA":
            new_key = JWK.generate_rsa_key(alg=algorithm)
        elif key_type == "oct":
            new_key = JWK.generate_hmac_key(alg=algorithm)
        else:
            raise ValueError(f"Unsupported key type: {key_type}")
        
        # Add the new key
        self.add_key(new_key)
        
        # Set as primary signing key
        old_primary = self.primary_signing_key
        self.primary_signing_key = new_key.kid
        
        # If there was a previous key, update its expiry for overlap
        if old_primary and old_primary in self.keys:
            old_key = self.keys[old_primary]
            
            # Set expiry to current time + overlap period
            old_key.expiry = datetime.now() + timedelta(days=overlap_days)
            
        # Save changes
        self.save_keys()
    
    def clean_expired_keys(self) -> None:
        """Remove all expired keys."""
        now = datetime.now()
        expired_kids = [
            kid for kid, key in self.keys.items()
            if key.expiry and now > key.expiry
        ]
        
        for kid in expired_kids:
            self.remove_key(kid)


def example_usage():
    """Demonstrate key management and rotation."""
    # Set up key manager
    key_manager = KeyManager("./jwks.json")
    
    # Initialize with a new key if empty
    if not key_manager.get_signing_key():
        # Generate an initial RSA key
        key_manager.rotate_keys(key_type="RSA", algorithm="RS256")
        print("Generated initial RSA key")
        
    # Get the current signing key
    signing_key = key_manager.get_signing_key()
    if signing_key:
        print(f"Current signing key: {signing_key.kid} ({signing_key.alg})")
        
        # Print when it expires
        if signing_key.expiry:
            print(f"Expires: {signing_key.expiry.isoformat()}")
    
    # Simulate key rotation
    print("\nRotating keys...")
    key_manager.rotate_keys(overlap_days=2)
    
    # Show all keys
    print("\nAll active keys:")
    for kid, key in key_manager.keys.items():
        status = "PRIMARY" if kid == key_manager.primary_signing_key else "BACKUP"
        valid_until = key.expiry.isoformat() if key.expiry else "No expiry"
        print(f"- {kid} ({key.alg}): {status}, valid until {valid_until}")
    
    # Export public JWKS for verification
    jwks_public = key_manager.get_jwks_public()
    print(f"\nPublic JWKS contains {len(jwks_public['keys'])} keys")
    
    # Clean expired keys
    print("\nCleaning expired keys...")
    key_manager.clean_expired_keys()


if __name__ == "__main__":
    example_usage()