/**
 * WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
 * NEVER use this approach for authentication or authorization in production.
 * ALWAYS verify the JWT signature before trusting any data in the token.
 */
// Using built-in modules
const authHeader = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
const token = authHeader.split(' ')[1];
const headerPart = token.split('.')[0];

// Convert Base64Url to Base64
const base64 = headerPart.replace(/-/g, '+').replace(/_/g, '/');
  
// Add padding if needed
const padLength = 4 - (base64.length % 4) % 4;
const paddedBase64 = base64 + '='.repeat(padLength);

const decodedHeader = JSON.parse(
  Buffer.from(paddedBase64, 'base64').toString()
);
console.log(decodedHeader);

// SECURE ALTERNATIVE:
// Using jwt library with proper verification
// const jwt = require('jsonwebtoken');
// function secureJwtParsing(token, secretKey) {
//   try {
//     // VERIFY first, then access data
//     const verified = jwt.verify(token, secretKey, { 
//       algorithms: ['HS256']  // Explicitly specify allowed algorithms
//     });
//     // Now it's safe to access headers
//     const header = jwt.decode(token, { complete: true }).header;
//     return { valid: true, header, payload: verified };
//   } catch (error) {
//     return { valid: false, error: error.message };
//   }
// }
