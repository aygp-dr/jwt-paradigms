/**
 * WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
 * NEVER use this approach for authentication or authorization in production.
 * ALWAYS verify the JWT signature before trusting any data in the token.
 */
const authHeader = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
const token = authHeader.split(' ')[1];

// Decode the header part
const headerPart = token.split('.')[0];

// Convert Base64Url to Base64
const base64 = headerPart.replace(/-/g, '+').replace(/_/g, '/');
  
// Add padding if needed
const padLength = 4 - (base64.length % 4) % 4;
const paddedBase64 = base64 + '='.repeat(padLength);

// Decode
const decodedHeader = JSON.parse(atob(paddedBase64));
console.log(decodedHeader);

// SECURE ALTERNATIVE:
// Use a proper JWT library that verifies signatures first
// import * as jose from 'jose';
// async function securelyVerifyJwt(token, secretKey) {
//   try {
//     const { payload, protectedHeader } = await jose.jwtVerify(
//       token, 
//       secretKey,
//       { algorithms: ['HS256'] }
//     );
//     return { valid: true, header: protectedHeader, payload };
//   } catch (error) {
//     return { valid: false, error: error.message };
//   }
// }
