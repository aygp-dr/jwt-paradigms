import jwt
from typing import Dict, Any, TypedDict, Optional, Union, List

class JwtVerifyResult(TypedDict, total=False):
    valid: bool
    header: Optional[Dict[str, Any]]
    payload: Optional[Dict[str, Any]]
    error: Optional[str]

def secure_jwt_validation(auth_header: str, secret_key: str, 
                        issuer: Optional[str] = None,
                        audience: Optional[str] = None) -> JwtVerifyResult:
    """
    Correct approach: verify signature before parsing payload
    
    Parameters:
    - auth_header: The Authorization header containing the JWT
    - secret_key: The key used to verify the signature
    - issuer: Expected issuer to validate against
    - audience: Expected audience to validate against
    
    Returns:
    A dictionary with validation results
    """
    try:
        # Extract token
        if not auth_header.startswith('Bearer '):
            raise ValueError("Missing Bearer prefix")
            
        token = auth_header.split(' ')[1]
        
        # Set up verification options
        options = {
            'verify_signature': True,  # Must verify signature
            'verify_exp': True,        # Check expiration
            'verify_nbf': True,        # Check not-before
            'verify_iat': True,        # Check issued-at
            'verify_aud': audience is not None,  # Check audience if provided
            'verify_iss': issuer is not None,    # Check issuer if provided
        }
        
        # Set up validation parameters
        kwargs = {}
        if issuer:
            kwargs['issuer'] = issuer
        if audience:
            kwargs['audience'] = audience
            
        # Explicitly set allowed algorithms (prevent algorithm confusion)
        algorithms = ['HS256']  # Only allow HMAC-SHA256
        
        # CRITICAL: Verify signature first
        # This prevents attack vectors like "alg":"none"
        payload = jwt.decode(
            token, 
            secret_key, 
            algorithms=algorithms,
            options=options,
            **kwargs
        )
        
        # Only after successful verification, get the header
        header = jwt.get_unverified_header(token)
        
        # Additional validation checks
        validate_token_claims(payload)
        
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
        
def validate_token_claims(payload: Dict[str, Any]) -> None:
    """Validate the required claims are present and valid"""
    required_claims = ['sub', 'exp', 'iat']
    
    for claim in required_claims:
        if claim not in payload:
            raise jwt.InvalidTokenError(f"Missing required claim: {claim}")
    
    # Implement additional business logic validations here
    # For example, validating permissions, roles, etc.
    
# Example of validating an entire authorization flow
def validate_auth_flow(auth_header: str, secret_key: str) -> Dict[str, Any]:
    """A more complete authorization validation flow"""
    # Verify the token
    result = secure_jwt_validation(
        auth_header, 
        secret_key,
        issuer="your-auth-service",
        audience="your-api"
    )
    
    if not result["valid"]:
        return {"authorized": False, "reason": result["error"]}
    
    # Extract necessary claims
    try:
        subject = result["payload"]["sub"]
        roles = result["payload"].get("roles", [])
        permissions = result["payload"].get("permissions", [])
        
        # Example of actual authorization logic
        is_admin = "admin" in roles
        can_read_users = "read:users" in permissions
        
        return {
            "authorized": True,
            "user_id": subject,
            "is_admin": is_admin,
            "can_read_users": can_read_users,
            "roles": roles,
            "permissions": permissions
        }
    except KeyError as e:
        return {"authorized": False, "reason": f"Missing critical claim: {str(e)}"}
