def secure_jwt_validation(auth_header):
    """Correct approach: verify signature before parsing payload"""
    try:
        # Extract token
        token = auth_header.split(' ')[1]
        
        # CRITICAL: Verify signature first
        # This prevents attack vectors like "alg":"none"
        verified_token = jwt_library.verify_signature(token, public_key)
        
        # Only after verification, decode and use the claims
        decoded = jwt_library.decode_verified_token(verified_token)
        
        return {"valid": True, "payload": decoded}
    except Exception as e:
        return {"valid": False, "error": str(e)}
