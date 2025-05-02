#!/bin/bash
# WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
# NEVER use this approach for authentication or authorization in production.
# ALWAYS verify the JWT signature before trusting any data in the token.

# Extract JWT from Authorization header
AUTH_HEADER="Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"
TOKEN=$(echo $AUTH_HEADER | cut -d' ' -f2)

# Extract header part (first part before the first dot)
HEADER_PART=$(echo $TOKEN | cut -d. -f1)

# Decode with base64 (add padding if needed)
PADDING=$(( 4 - ((${#HEADER_PART} % 4) % 4) ))
if [ $PADDING -ne 4 ]; then
  HEADER_PART="${HEADER_PART}$(printf '=%.0s' $(seq 1 $PADDING))"
fi

# Decode and parse with jq
# Replace URL-safe characters with standard base64 characters
DECODED_HEADER=$(echo $HEADER_PART | tr '_-' '/+' | base64 -d | jq .)

# Validate required fields
ALG=$(echo $DECODED_HEADER | jq -r '.alg')
TYP=$(echo $DECODED_HEADER | jq -r '.typ')

if [ "$ALG" == "null" ]; then
  echo "Error: JWT header missing 'alg' field" >&2
  exit 1
fi

if [ "$TYP" == "null" ]; then
  echo "Error: JWT header missing 'typ' field" >&2
  exit 1
fi

echo $DECODED_HEADER

# SECURE ALTERNATIVE:
# In shell scripts, it's better to use a dedicated JWT tool like the 'jwt' command line
# tool (https://github.com/mike-engel/jwt-cli) or call a proper JWT library
# through Python/Node.js rather than implementing token verification manually.
