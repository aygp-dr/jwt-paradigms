import base64
import json

def parse_jwt(token):
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

# Example usage
if __name__ == "__main__":
    # Sample token (DO NOT use real tokens in code)
    token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    
    try:
        # INSECURE - only for demonstration!
        parsed = parse_jwt(token)
        print("⚠️ WARNING: This is unsafe parsing and should NEVER be used for authentication!")
        print(f"Header: {parsed['header']}")
        print(f"Payload: {parsed['payload']}")
    except Exception as e:
        print(f"Error parsing token: {e}")
