interface JwtHeader {
  alg: string;
  typ: string;
}

function decodeJwtHeader(authHeader: string): JwtHeader {
  const token: string = authHeader.split(' ')[1];
  const headerPart: string = token.split('.')[0];
  
  // Add padding if needed
  const base64 = headerPart.replace(/-/g, '+').replace(/_/g, '/');
  const padded = base64.padEnd(base64.length + (4 - (base64.length % 4)) % 4, '=');
  
  const decodedHeader: JwtHeader = JSON.parse(
    Buffer.from(padded, 'base64').toString()
  );
  return decodedHeader;
}

const authHeader = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
console.log(decodeJwtHeader(authHeader));
