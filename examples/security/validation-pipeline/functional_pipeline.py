"""
Functional JWT Validation Pipeline

This module demonstrates a secure JWT validation pipeline using a functional approach:
1. Composable validation functions
2. Proper validation order (signature -> time claims -> issuer/audience -> custom)
3. Early failure for critical checks
4. Immutable data transformation

Each validation function takes a JWT context object and returns either a valid
context or raises an exception, making it easy to compose validation steps.
"""

import base64
import hmac
import json
import time
from typing import Any, Callable, Dict, List, NamedTuple, Optional, TypeVar, Union, cast
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding, rsa
from cryptography.exceptions import InvalidSignature
from functools import reduce


# Type definitions for strong typing
T = TypeVar('T')  # Generic type for pipeline functions

# Custom exception type for all JWT validation errors
class JWTValidationError(Exception):
    """Base exception for all JWT validation errors."""
    pass


class JWTContext(NamedTuple):
    """Immutable JWT validation context.
    
    This class holds the state of JWT validation as it passes through
    the pipeline of validation functions.
    """
    raw_token: str
    parts: List[str]
    header: Dict[str, Any]
    payload: Dict[str, Any]
    signature: bytes
    keys: Dict[str, Any]
    options: Dict[str, Any]
    signature_verified: bool = False
    time_claims_verified: bool = False
    issuer_verified: bool = False
    audience_verified: bool = False
    custom_verified: bool = False


# Helper functions for base64url decoding
def base64url_decode(data: str) -> bytes:
    """Decode base64url-encoded data.
    
    Args:
        data: Base64url-encoded string
    
    Returns:
        Decoded bytes
    """
    # Add padding if needed
    padding_needed = len(data) % 4
    if padding_needed:
        data += '=' * (4 - padding_needed)
        
    # Replace URL-safe characters with standard base64 characters
    data = data.replace('-', '+').replace('_', '/')
    
    return base64.b64decode(data)


def parse_token(token: str, keys: Dict[str, Any], options: Dict[str, Any]) -> JWTContext:
    """Parse JWT token into its parts without validation.
    
    Args:
        token: JWT token string
        keys: Dictionary of keys for signature verification
        options: Validation options
    
    Returns:
        JWTContext with parsed token parts
    
    Raises:
        JWTValidationError: If token format is invalid
    """
    parts = token.split('.')
    if len(parts) != 3:
        raise JWTValidationError("Token must have three parts")
    
    try:
        header_part, payload_part, signature_part = parts
        
        # Decode header
        header_json = base64url_decode(header_part).decode('utf-8')
        header = json.loads(header_json)
        
        # Decode payload
        payload_json = base64url_decode(payload_part).decode('utf-8')
        payload = json.loads(payload_json)
        
        # Decode signature
        signature = base64url_decode(signature_part)
        
        return JWTContext(
            raw_token=token,
            parts=parts,
            header=header,
            payload=payload,
            signature=signature,
            keys=keys,
            options=options
        )
    except (ValueError, json.JSONDecodeError) as e:
        raise JWTValidationError(f"Failed to parse token: {str(e)}")


def verify_algorithm(ctx: JWTContext) -> JWTContext:
    """Verify the token algorithm is allowed.
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context
    
    Raises:
        JWTValidationError: If algorithm is not allowed
    """
    alg = ctx.header.get('alg')
    allowed_algorithms = ctx.options.get('algorithms', ['HS256', 'RS256'])
    
    if not alg:
        raise JWTValidationError("Token header missing 'alg' claim")
    
    if alg not in allowed_algorithms:
        raise JWTValidationError(f"Algorithm '{alg}' not allowed")
    
    # "none" algorithm is explicitly rejected, even if in allowed list
    if alg == 'none':
        raise JWTValidationError("'none' algorithm is not allowed")
    
    return ctx


def verify_signature(ctx: JWTContext) -> JWTContext:
    """Verify token signature.
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context with signature_verified=True
    
    Raises:
        JWTValidationError: If signature is invalid
    """
    alg = ctx.header.get('alg')
    kid = ctx.header.get('kid')
    
    # Get key
    key = ctx.keys.get(kid or 'default')
    if not key:
        raise JWTValidationError(f"Key not found for kid: {kid or 'default'}")
    
    # Ensure algorithm matches key
    if hasattr(key, 'alg') and key.alg != alg:
        raise JWTValidationError(f"Algorithm mismatch: key is for {key.alg}, token uses {alg}")
    
    # Message to verify is header.payload
    message = f"{ctx.parts[0]}.{ctx.parts[1]}".encode()
    
    try:
        # Verify based on algorithm
        if alg == 'HS256':
            # HMAC-SHA256
            expected_signature = hmac.new(
                key if isinstance(key, bytes) else key.encode(),
                message,
                hashes.SHA256()
            ).digest()
            
            if not hmac.compare_digest(ctx.signature, expected_signature):
                raise JWTValidationError("Invalid signature")
                
        elif alg == 'RS256':
            # RSA-SHA256
            # Assuming key is a cryptography RSA public key
            key.verify(
                ctx.signature,
                message,
                padding.PKCS1v15(),
                hashes.SHA256()
            )
        
        else:
            # This shouldn't happen due to algorithm verification
            raise JWTValidationError(f"Algorithm verification not implemented: {alg}")
            
    except InvalidSignature:
        raise JWTValidationError("Invalid signature")
    
    # Return updated context with signature verified
    return ctx._replace(signature_verified=True)


def verify_time_claims(ctx: JWTContext) -> JWTContext:
    """Verify token time-related claims (exp, nbf, iat).
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context with time_claims_verified=True
    
    Raises:
        JWTValidationError: If time claims are invalid
    """
    # Skip verification if disabled in options
    if ctx.options.get('verify_exp', True) is False and \
       ctx.options.get('verify_nbf', True) is False and \
       ctx.options.get('verify_iat', True) is False:
        return ctx._replace(time_claims_verified=True)
    
    current_time = time.time()
    leeway = ctx.options.get('leeway', 0)  # Time leeway in seconds
    
    # Check expiration
    if ctx.options.get('verify_exp', True) and 'exp' in ctx.payload:
        exp = ctx.payload['exp']
        if not isinstance(exp, (int, float)):
            raise JWTValidationError("'exp' claim must be a number")
        
        if current_time > exp + leeway:
            raise JWTValidationError("Token has expired")
    
    # Check not-before time
    if ctx.options.get('verify_nbf', True) and 'nbf' in ctx.payload:
        nbf = ctx.payload['nbf']
        if not isinstance(nbf, (int, float)):
            raise JWTValidationError("'nbf' claim must be a number")
        
        if current_time < nbf - leeway:
            raise JWTValidationError("Token not yet valid")
    
    # Check issued-at time for token freshness
    if ctx.options.get('verify_iat', True) and 'iat' in ctx.payload:
        iat = ctx.payload['iat']
        if not isinstance(iat, (int, float)):
            raise JWTValidationError("'iat' claim must be a number")
        
        max_age = ctx.options.get('max_age')
        if max_age is not None and current_time > iat + max_age:
            raise JWTValidationError("Token is too old")
    
    return ctx._replace(time_claims_verified=True)


def verify_issuer(ctx: JWTContext) -> JWTContext:
    """Verify token issuer claim.
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context with issuer_verified=True
    
    Raises:
        JWTValidationError: If issuer is invalid
    """
    # Skip verification if disabled in options
    if ctx.options.get('verify_iss', True) is False:
        return ctx._replace(issuer_verified=True)
    
    expected_issuer = ctx.options.get('issuer')
    if expected_issuer:
        if 'iss' not in ctx.payload:
            raise JWTValidationError("Token missing required 'iss' claim")
        
        iss = ctx.payload['iss']
        
        # Allow list of issuers
        if isinstance(expected_issuer, list):
            if iss not in expected_issuer:
                raise JWTValidationError(f"Invalid issuer: {iss}")
        else:
            if iss != expected_issuer:
                raise JWTValidationError(f"Invalid issuer: {iss}")
    
    return ctx._replace(issuer_verified=True)


def verify_audience(ctx: JWTContext) -> JWTContext:
    """Verify token audience claim.
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context with audience_verified=True
    
    Raises:
        JWTValidationError: If audience is invalid
    """
    # Skip verification if disabled in options
    if ctx.options.get('verify_aud', True) is False:
        return ctx._replace(audience_verified=True)
    
    expected_audience = ctx.options.get('audience')
    if expected_audience:
        if 'aud' not in ctx.payload:
            raise JWTValidationError("Token missing required 'aud' claim")
        
        aud = ctx.payload['aud']
        
        # Handle both string and list audience in token
        if isinstance(aud, list):
            # Check if any audience in token matches any expected audience
            if isinstance(expected_audience, list):
                if not any(a in expected_audience for a in aud):
                    raise JWTValidationError(f"Invalid audience: {aud}")
            else:
                if expected_audience not in aud:
                    raise JWTValidationError(f"Invalid audience: {aud}")
        else:
            # Audience in token is a string
            if isinstance(expected_audience, list):
                if aud not in expected_audience:
                    raise JWTValidationError(f"Invalid audience: {aud}")
            else:
                if aud != expected_audience:
                    raise JWTValidationError(f"Invalid audience: {aud}")
    
    return ctx._replace(audience_verified=True)


def verify_custom_claims(ctx: JWTContext) -> JWTContext:
    """Verify custom token claims.
    
    Args:
        ctx: JWT validation context
    
    Returns:
        Updated JWT context with custom_verified=True
    
    Raises:
        JWTValidationError: If custom claims are invalid
    """
    # Get custom claim verifiers from options
    custom_validators = ctx.options.get('custom_validators', [])
    
    # Apply each validator
    for validator in custom_validators:
        validator(ctx.payload)
    
    return ctx._replace(custom_verified=True)


def verify_token(
    token: str,
    keys: Dict[str, Any],
    options: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """Verify JWT token using a functional pipeline.
    
    This function applies a series of validation functions in the correct order:
    1. Parse token
    2. Verify algorithm
    3. Verify signature (critical)
    4. Verify time claims (exp, nbf, iat)
    5. Verify issuer
    6. Verify audience
    7. Verify custom claims
    
    Args:
        token: JWT token string
        keys: Dictionary of keys for signature verification
        options: Validation options
    
    Returns:
        Verified token payload
    
    Raises:
        JWTValidationError: If token is invalid
    """
    options = options or {}
    
    # Define the validation pipeline - order matters!
    pipeline = [
        verify_algorithm,
        verify_signature,      # Critical - must happen before accessing payload
        verify_time_claims,    # Time-based claims
        verify_issuer,         # Issuer verification
        verify_audience,       # Audience verification
        verify_custom_claims,  # Custom claim verification
    ]
    
    try:
        # Parse the token first
        ctx = parse_token(token, keys, options)
        
        # Apply the validation pipeline
        verified_ctx = reduce(lambda c, f: f(c), pipeline, ctx)
        
        # Ensure critical validations were performed
        assert verified_ctx.signature_verified, "Signature not verified"
        
        return verified_ctx.payload
    except JWTValidationError as e:
        # Pass through validation errors
        raise
    except Exception as e:
        # Convert other errors to validation errors
        raise JWTValidationError(f"Token validation failed: {str(e)}")


# Example of a custom claim validator
def require_claim(claim_name: str) -> Callable[[Dict[str, Any]], None]:
    """Create a validator that requires a specific claim.
    
    Args:
        claim_name: Name of the required claim
    
    Returns:
        Validator function
    """
    def validator(payload: Dict[str, Any]) -> None:
        if claim_name not in payload:
            raise JWTValidationError(f"Token missing required claim: {claim_name}")
    return validator


# Example usage
def example_usage():
    # Sample token (example only, not a real token)
    sample_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    
    # Sample keys
    keys = {
        'default': 'your-256-bit-secret'
    }
    
    # Sample options
    options = {
        'algorithms': ['HS256'],
        'issuer': 'https://your-trusted-issuer.com',
        'audience': 'your-service-id',
        'leeway': 30,  # 30 seconds leeway for clock skew
        'max_age': 3600,  # Maximum token age of 1 hour
        'custom_validators': [
            require_claim('sub'),  # Require subject claim
        ]
    }
    
    try:
        # Verify token
        payload = verify_token(sample_token, keys, options)
        print(f"Verified token for subject: {payload.get('sub')}")
    except JWTValidationError as e:
        print(f"Token validation failed: {str(e)}")


if __name__ == "__main__":
    example_usage()