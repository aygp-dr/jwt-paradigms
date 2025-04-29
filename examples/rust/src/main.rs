use base64::{engine::general_purpose, Engine as _};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Serialize, Deserialize)]
struct JwtHeader {
    alg: String,
    typ: String,
}

fn decode_jwt_header(auth_header: &str) -> Result<JwtHeader, Box<dyn std::error::Error>> {
    let token = auth_header.split_whitespace().nth(1).ok_or("Invalid auth header")?;
    let header_part = token.split('.').next().ok_or("Invalid token format")?;
    
    // Decode base64url to bytes
    let decoded_bytes = general_purpose::URL_SAFE_NO_PAD.decode(header_part)?;
    
    // Parse JSON
    let header: JwtHeader = serde_json::from_slice(&decoded_bytes)?;
    Ok(header)
}

fn main() {
    let auth_header = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";
    
    match decode_jwt_header(auth_header) {
        Ok(header) => println!("{:?}", header),
        Err(err) => eprintln!("Error: {}", err),
    }
}
