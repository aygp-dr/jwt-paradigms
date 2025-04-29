#!/bin/bash

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
DECODED_HEADER=$(echo $HEADER_PART | tr '_-' '/+' | base64 -d | jq .)
echo $DECODED_HEADER
