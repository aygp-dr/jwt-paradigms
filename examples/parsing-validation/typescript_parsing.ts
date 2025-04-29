/**
 * UNSAFE: Parse a JWT without signature verification
 * Only use for debugging or examining token structure
 * NEVER use for authentication decisions
 */
function parseJwt(token: string): {
  header: any;
  payload: any;
  signatureSegment: string;
  isVerified: boolean;
} {
  // Split the token into parts
  const parts = token.split('.');
  if (parts.length !== 3) {
    throw new Error('Invalid JWT format');
  }

  // Base64url decode function
  const decodeSegment = (segment: string): any => {
    // Add padding if needed
    const padded = segment.padEnd(
      segment.length + (4 - (segment.length % 4)) % 4,
      '='
    );
    
    // Convert from URL-safe base64 to standard base64
    const base64 = padded.replace(/-/g, '+').replace(/_/g, '/');
    
    // Decode
    try {
      const decoded = Buffer.from(base64, 'base64').toString();
      return JSON.parse(decoded);
    } catch (e) {
      throw new Error(`Failed to decode JWT segment: ${e}`);
    }
  };

  // Parse header and payload
  const header = decodeSegment(parts[0]);
  const payload = decodeSegment(parts[1]);

  // Return parsed data (note: signature is NOT verified)
  return {
    header,
    payload,
    signatureSegment: parts[2], // Raw, still encoded
    isVerified: false // Explicitly mark as unverified
  };
}

// Function to demonstrate the dangers of parsing without verification
function demonstrateParsingDanger(): void {
  // Create a completely fake token with admin privileges
  const maliciousHeader = { alg: 'HS256', typ: 'JWT' };
  const maliciousPayload = { 
    sub: 'attacker',
    role: 'admin',
    permissions: ['*'],
    exp: Math.floor(Date.now() / 1000) + 3600
  };

  // Encode to base64url
  const encodeSegment = (obj: any): string => {
    const json = JSON.stringify(obj);
    const base64 = Buffer.from(json).toString('base64');
    return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
  };

  const fakeToken = `${encodeSegment(maliciousHeader)}.${encodeSegment(maliciousPayload)}.totallyFakeSignature`;
  
  console.log('Fake token created by attacker:', fakeToken);
  
  // UNSAFE parsing - this will "successfully" parse the fake token!
  const parsed = parseJwt(fakeToken);
  console.log('⚠️ DANGER: Parsed payload without verification:', parsed.payload);
  
  console.log('\nThis highlights why you should NEVER rely on parsing alone for authorization decisions.');
}

// Example usage
const token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c";

try {
  // INSECURE - only for demonstration!
  const parsed = parseJwt(token);
  console.log("⚠️ WARNING: This is unsafe parsing and should NEVER be used for authentication!");
  console.log("Header:", parsed.header);
  console.log("Payload:", parsed.payload);
  
  // Demonstrate the dangers
  console.log("\n--- Security Vulnerability Demonstration ---");
  demonstrateParsingDanger();
} catch (e) {
  console.error("Error parsing token:", e);
}
