const authHeader = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
const token = authHeader.split(' ')[1];

// Decode the header part
const headerPart = token.split('.')[0];
const decodedHeader = JSON.parse(atob(headerPart));
console.log(decodedHeader);
