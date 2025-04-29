use jsonwebtoken::{decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::env;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

// Configuration constants
const SERVICE_ID: &str = "analytics-service";
const TTL_SECONDS: u64 = 300; // 5 minutes

#[derive(Debug, Serialize, Deserialize)]
struct Context {
    req_id: String,
    source_ip: String,
    #[serde(flatten)]
    additional: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ProxyTokenClaims {
    sub: String,      // Original user ID
    iss: String,      // Issuing service
    aud: String,      // Target service
    orig: String,     // Original token fingerprint
    iat: u64,         // Issued at time
    exp: u64,         // Expiration time
    scope: Vec<String>, // Allowed actions
    ctx: Context,     // Context data
}

/// Generate a proxy token for making downstream service calls
fn generate_proxy_token(
    user_id: &str,
    original_token: &str,
    target_service: &str,
    scopes: Vec<String>,
    request_context: HashMap<String, serde_json::Value>,
) -> Result<String, jsonwebtoken::errors::Error> {
    // Create fingerprint of original token to prevent token substitution attacks
    let mut hasher = Sha256::new();
    hasher.update(original_token.as_bytes());
    let token_fingerprint = format!("{:x}", hasher.finalize());
    
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs();
    
    // Extract specific fields or set defaults
    let req_id = match request_context.get("requestId") {
        Some(serde_json::Value::String(id)) => id.clone(),
        _ => Uuid::new_v4().to_string(),
    };
    
    let source_ip = match request_context.get("ip") {
        Some(serde_json::Value::String(ip)) => ip.clone(),
        _ => "0.0.0.0".to_string(),
    };
    
    // Create a new HashMap for additional context, excluding fields we've already used
    let mut additional_ctx = request_context.clone();
    additional_ctx.remove("requestId");
    additional_ctx.remove("ip");
    
    let claims = ProxyTokenClaims {
        sub: user_id.to_string(),
        iss: SERVICE_ID.to_string(),
        aud: target_service.to_string(),
        orig: token_fingerprint,
        iat: now,
        exp: now + TTL_SECONDS,
        scope: scopes,
        ctx: Context {
            req_id,
            source_ip,
            additional: additional_ctx,
        },
    };
    
    let secret_key = env::var("JWT_SECRET").unwrap_or_else(|_| "your-secret-key".to_string());
    encode(
        &Header::new(Algorithm::HS256),
        &claims,
        &EncodingKey::from_secret(secret_key.as_bytes()),
    )
}

/// Verify a proxy token in a downstream service
fn verify_proxy_token(
    token: &str,
    expected_audience: &str,
) -> Result<ProxyTokenClaims, jsonwebtoken::errors::Error> {
    let secret_key = env::var("JWT_SECRET").unwrap_or_else(|_| "your-secret-key".to_string());
    
    let mut validation = Validation::new(Algorithm::HS256);
    validation.set_audience(&[expected_audience]);
    
    let token_data = decode::<ProxyTokenClaims>(
        token,
        &DecodingKey::from_secret(secret_key.as_bytes()),
        &validation,
    )?;
    
    Ok(token_data.claims)
}

fn main() {
    // Example usage
    let original_user_token = "eyJhbGciOiJIUzI1NiIsInR...";
    let user_id = "user456";
    let target_service = "payment-service";
    let allowed_scopes = vec![
        "read:transactions".to_string(),
        "process:refund".to_string(),
    ];
    
    // Create a request context
    let mut request_context = HashMap::new();
    request_context.insert("requestId".to_string(), serde_json::Value::String("req_12345".to_string()));
    request_context.insert("ip".to_string(), serde_json::Value::String("192.168.1.100".to_string()));
    request_context.insert("userAgent".to_string(), serde_json::Value::String("Mozilla/5.0...".to_string()));
    request_context.insert("route".to_string(), serde_json::Value::String("/api/transactions".to_string()));
    
    match generate_proxy_token(
        user_id,
        original_user_token,
        target_service,
        allowed_scopes,
        request_context,
    ) {
        Ok(proxy_token) => {
            println!("Proxy Token: {}", proxy_token);
            
            // Simulate verification in the target service
            match verify_proxy_token(&proxy_token, target_service) {
                Ok(verification_result) => {
                    println!("Verification successful: {:?}", verification_result);
                }
                Err(e) => {
                    println!("Verification failed: {}", e);
                }
            }
        }
        Err(e) => {
            println!("Token generation failed: {}", e);
        }
    }
}
