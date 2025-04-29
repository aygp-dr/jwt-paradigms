// Using built-in modules
const authHeader = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
const token = authHeader.split(' ')[1];
const headerPart = token.split('.')[0];
const decodedHeader = JSON.parse(
  Buffer.from(headerPart, 'base64').toString()
);
console.log(decodedHeader);

// Using jwt library
// Uncomment to use jwt package
// const jwt = require('jsonwebtoken');
// const decoded = jwt.decode(token, {complete: true});
// console.log(decoded.header);
