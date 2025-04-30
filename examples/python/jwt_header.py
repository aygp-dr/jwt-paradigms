#!/usr/bin/env python3

import base64
import json

def decode_jwt_header(auth_header):
    token = auth_header.split(' ')[1]
    header_part = token.split('.')[0]
    
    # Add padding if needed
    padding_needed = len(header_part) % 4
    if padding_needed:
        header_part += '=' * (4 - padding_needed)
    
    # Decode base64
    decoded_bytes = base64.b64decode(header_part.replace('-', '+').replace('_', '/'))
    decoded_str = decoded_bytes.decode('utf-8')
    
    # Parse JSON
    return json.loads(decoded_str)

auth_header = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"
print(decode_jwt_header(auth_header))

# Uncomment to use PyJWT
# import jwt
# decoded = jwt.decode(auth_header.split(' ')[1], options={"verify_signature": False})
# print(jwt.get_unverified_header(auth_header.split(' ')[1]))
