use base64::{engine::general_purpose, Engine as _};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;

#[derive(Debug, Serialize, Deserialize)]
struct JwtHeader {
    alg: String,
    typ: String,
}

#[derive(Error, Debug)]
enum JwtError {
    #[error("Invalid auth header format")]
    InvalidAuthHeader,
    
    #[error("Invalid token format")]
    InvalidTokenFormat,
    
    #[error("Base64 decoding error: {0}")]
    Base64Error(#[from] base64::DecodeError),
    
    #[error("JSON parsing error: {0}")]
    JsonError(#[from] serde_json::Error),
}

// WARNING: This example demonstrates JWT header parsing WITHOUT signature verification.
// NEVER use this approach for authentication or authorization in production.
// ALWAYS verify the JWT signature before trusting any data in the token.
fn decode_jwt_header(auth_header: &str) -> Result<JwtHeader, JwtError> {
    // Extract token from auth header
    let token = auth_header
        .split_whitespace()
        .nth(1)
        .ok_or(JwtError::InvalidAuthHeader)?;
    
    // Extract header part
    let header_part = token
        .split('.')
        .next()
        .ok_or(JwtError::InvalidTokenFormat)?;
    
    // Decode base64url to bytes - Rust's base64 crate handles URL-safe format natively
    let decoded_bytes = general_purpose::URL_SAFE_NO_PAD.decode(header_part)?;
    
    // Parse JSON
    let header: JwtHeader = serde_json::from_slice(&decoded_bytes)?;
    Ok(header)
}

// SECURE ALTERNATIVE:
// use jsonwebtoken::{decode, DecodingKey, Validation, Algorithm};
// 
// fn securely_verify_jwt(token: &str, secret: &[u8]) -> Result<(JwtHeader, Value), String> {
//     // Set up validation with explicitly allowed algorithms
//     let mut validation = Validation::new(Algorithm::HS256);
//     validation.validate_exp = true;
//     validation.validate_nbf = true;
//     
//     // Verify first - this checks signature, expiration, etc.
//     match decode::<Value>(token, &DecodingKey::from_secret(secret), &validation) {
//         Ok(data) => {
//             // Now we can extract the header
//             let header = data.header;
//             let claims = data.claims;
//             
//             // Convert generic header to our type
//             let header = JwtHeader {
//                 alg: header.alg.to_string(),
//                 typ: header.typ.unwrap_or_else(|| "JWT".to_string()),
//             };
//             
//             Ok((header, claims))
//         },
//         Err(err) => Err(format!("Token validation failed: {}", err)),
//     }
// }

fn main() {
    let auth_header = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
    
    match decode_jwt_header(auth_header) {
        Ok(header) => println!("{:?}", header),
        Err(err) => eprintln!("Error: {}", err),
    }
}
