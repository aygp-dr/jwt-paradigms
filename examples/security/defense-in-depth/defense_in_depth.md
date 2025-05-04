# Defense-in-Depth Strategies for JWT Security

JWT validation alone is insufficient for robust security in production systems. This document outlines a comprehensive defense-in-depth approach combining JWT validation with additional security controls.

## Core Principles

1. **No Single Point of Failure**: Security should not depend solely on JWT validation
2. **Multiple Layers of Protection**: Combine independent security mechanisms
3. **Principle of Least Privilege**: Minimize access rights for each component
4. **Detection Alongside Prevention**: Monitor for suspicious activity
5. **Secure Failure Modes**: Fail securely when components break

## Defense Layers

### Layer 1: Secure JWT Implementation

* Strong signature validation (covered in other examples)
* Proper claim validation 
* Algorithm restrictions
* Short token lifetimes
* Secure key management

### Layer 2: Rate Limiting and Traffic Management

```python
# Example rate limiting middleware (Python/Flask)
from flask import Flask, request, jsonify
import time
import redis

app = Flask(__name__)
rate_limiter = redis.Redis(host='localhost', port=6379, db=0)

def rate_limit(key_prefix, limit=10, period=60):
    """Rate limiting using Redis sliding window."""
    def decorator(f):
        def wrapped_function(*args, **kwargs):
            # Get client identifier (IP, user ID, etc.)
            client_id = request.headers.get('X-Forwarded-For', request.remote_addr)
            key = f"{key_prefix}:{client_id}"
            
            # Get current count
            current = rate_limiter.get(key)
            if current is not None and int(current) >= limit:
                return jsonify({'error': 'Rate limit exceeded'}), 429
            
            # Increment and set expiry
            pipe = rate_limiter.pipeline()
            pipe.incr(key)
            pipe.expire(key, period)
            pipe.execute()
            
            return f(*args, **kwargs)
        return wrapped_function
    return decorator

@app.route('/api/protected')
@rate_limit('api-calls', limit=10, period=60)  # 10 requests per minute
def protected_endpoint():
    # JWT validation would happen here
    return jsonify({'data': 'Protected resource'})
```

### Layer 3: Context-Based Authentication

```typescript
// TypeScript example of context validation
interface AuthContext {
  token: string;
  ipAddress: string;
  userAgent: string;
  deviceId?: string;
  geoLocation?: {
    country: string;
    region: string;
  };
  requestTime: number;
}

class ContextValidator {
  private knownLocations: Map<string, string[]> = new Map();
  private knownDevices: Map<string, string[]> = new Map();

  constructor(private riskThreshold: number = 0.7) {
    // Initialize with known good contexts
  }

  public validateContext(context: AuthContext, userId: string): boolean {
    let riskScore = 0;

    // Check if IP is from a known location for this user
    if (context.geoLocation) {
      const knownCountries = this.knownLocations.get(userId) || [];
      if (!knownCountries.includes(context.geoLocation.country)) {
        riskScore += 0.4; // Significant risk factor
      }
    }

    // Check if device is known
    if (context.deviceId) {
      const knownDevices = this.knownDevices.get(userId) || [];
      if (!knownDevices.includes(context.deviceId)) {
        riskScore += 0.3; // Moderate risk factor
      }
    }

    // Check for unusual time of access
    const hour = new Date(context.requestTime).getHours();
    if (hour >= 1 && hour <= 5) { // Middle of the night
      riskScore += 0.2; // Minor risk factor
    }

    // Determine if the risk is acceptable
    return riskScore < this.riskThreshold;
  }
}
```

### Layer 4: Monitoring and Anomaly Detection

```python
# Python example of JWT usage monitoring
import json
import time
import statistics
from typing import Dict, List, Optional, Tuple

class JWTMonitor:
    def __init__(self, alert_threshold: float = 3.0):
        self.token_usage: Dict[str, List[Tuple[str, float]]] = {}  # user_id -> [(endpoint, timestamp)]
        self.baseline: Dict[str, Dict[str, float]] = {}  # user_id -> endpoint -> avg_requests_per_hour
        self.alert_threshold = alert_threshold
    
    def record_token_usage(self, user_id: str, endpoint: str) -> None:
        """Record token usage for anomaly detection."""
        if user_id not in self.token_usage:
            self.token_usage[user_id] = []
        
        self.token_usage[user_id].append((endpoint, time.time()))
    
    def calculate_baseline(self) -> None:
        """Calculate baseline usage patterns for each user."""
        for user_id, usages in self.token_usage.items():
            # Group by endpoint
            endpoint_usages: Dict[str, List[float]] = {}
            for endpoint, timestamp in usages:
                if endpoint not in endpoint_usages:
                    endpoint_usages[endpoint] = []
                endpoint_usages[endpoint].append(timestamp)
            
            # Calculate average requests per hour for each endpoint
            user_baseline: Dict[str, float] = {}
            for endpoint, timestamps in endpoint_usages.items():
                # Sort timestamps
                sorted_times = sorted(timestamps)
                if len(sorted_times) < 2:
                    continue
                
                # Calculate time range in hours
                time_range_hours = (sorted_times[-1] - sorted_times[0]) / 3600
                if time_range_hours < 1:
                    time_range_hours = 1  # Minimum 1 hour to avoid division by zero
                
                # Calculate requests per hour
                requests_per_hour = len(sorted_times) / time_range_hours
                user_baseline[endpoint] = requests_per_hour
            
            self.baseline[user_id] = user_baseline
    
    def check_anomaly(self, user_id: str, endpoint: str) -> Optional[str]:
        """Check if current usage is anomalous compared to baseline."""
        if user_id not in self.baseline or endpoint not in self.baseline[user_id]:
            return None  # Not enough data
        
        # Get baseline for this user and endpoint
        baseline_rate = self.baseline[user_id][endpoint]
        
        # Calculate current rate (requests in the last hour)
        now = time.time()
        one_hour_ago = now - 3600
        recent_requests = [t for e, t in self.token_usage[user_id] 
                          if e == endpoint and t > one_hour_ago]
        
        current_rate = len(recent_requests)
        
        # Check if rate exceeds threshold
        if current_rate > baseline_rate * self.alert_threshold:
            return f"Anomalous token usage: {current_rate} requests in the last hour vs baseline of {baseline_rate:.2f}"
        
        return None
```

### Layer 5: Token Revocation and Blacklisting

```python
# Python Redis-based token blacklist
import redis
import json
import time
from typing import Dict, Any, Optional

class TokenBlacklist:
    def __init__(self, redis_host: str = 'localhost', redis_port: int = 6379):
        self.redis = redis.Redis(host=redis_host, port=redis_port, db=0)
        self.prefix = 'token:blacklist:'
    
    def revoke_token(self, token_id: str, reason: str, expires_at: Optional[int] = None) -> None:
        """Add a token to the blacklist.
        
        Args:
            token_id: Unique identifier for the token (jti claim)
            reason: Reason for revocation
            expires_at: When the token would expire anyway (can be used for cleanup)
        """
        data = {
            'revoked_at': int(time.time()),
            'reason': reason
        }
        
        key = f"{self.prefix}{token_id}"
        self.redis.set(key, json.dumps(data))
        
        # Set expiry if provided (add buffer time after token expiry)
        if expires_at:
            # Keep in blacklist for 15 minutes after expiry as a buffer
            buffer_time = 15 * 60
            ttl = max(0, expires_at - int(time.time()) + buffer_time)
            self.redis.expire(key, ttl)
    
    def revoke_all_for_user(self, user_id: str, reason: str) -> None:
        """Revoke all tokens for a specific user.
        
        This requires maintaining a secondary index of tokens by user.
        """
        user_tokens_key = f"user:tokens:{user_id}"
        token_ids = self.redis.smembers(user_tokens_key)
        
        for token_id in token_ids:
            self.revoke_token(token_id.decode('utf-8'), reason)
    
    def is_revoked(self, token_id: str) -> bool:
        """Check if a token has been revoked."""
        key = f"{self.prefix}{token_id}"
        return self.redis.exists(key) == 1
    
    def get_revocation_info(self, token_id: str) -> Optional[Dict[str, Any]]:
        """Get information about a revoked token."""
        key = f"{self.prefix}{token_id}"
        data = self.redis.get(key)
        
        if data:
            return json.loads(data)
        return None
    
    def cleanup_expired(self) -> int:
        """Cleanup expired tokens from the blacklist.
        
        Returns:
            Number of tokens removed
        """
        # This would be called periodically by a maintenance job
        # For tokens without an expiry time set in Redis
        
        keys = self.redis.keys(f"{self.prefix}*")
        now = int(time.time())
        removed = 0
        
        for key in keys:
            data_str = self.redis.get(key)
            if not data_str:
                continue
                
            data = json.loads(data_str)
            
            # If we know when it expires and it's in the past, remove it
            if 'expires_at' in data and data['expires_at'] < now:
                self.redis.delete(key)
                removed += 1
        
        return removed
```

### Layer 6: Secure Headers and Response Controls

```typescript
// TypeScript middleware for secure headers
import { Request, Response, NextFunction } from 'express';

export function securityHeaders(req: Request, res: Response, next: NextFunction): void {
  // Prevent token leakage via Referer header
  res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');
  
  // Prevent response from being framed (clickjacking protection)
  res.setHeader('X-Frame-Options', 'DENY');
  
  // Enable XSS protection
  res.setHeader('X-XSS-Protection', '1; mode=block');
  
  // Prevent MIME type sniffing
  res.setHeader('X-Content-Type-Options', 'nosniff');
  
  // Content Security Policy
  res.setHeader('Content-Security-Policy', 
    "default-src 'self'; script-src 'self'; object-src 'none'; frame-ancestors 'none'");
  
  // HTTP Strict Transport Security (force HTTPS)
  res.setHeader('Strict-Transport-Security', 'max-age=31536000; includeSubDomains; preload');

  next();
}
```

### Layer 7: Infrastructure Security

- Use network segmentation to isolate authentication services
- Implement infrastructure-level DoS protection
- Apply Web Application Firewall (WAF) rules specifically targeting JWT-based attacks
- Configure proper TLS settings to prevent token interception

## Complete Implementation Examples

For complete implementation examples integrating all these layers, see:

1. [Python Defense-in-Depth Implementation](./python_defense_in_depth.py)
2. [TypeScript/Node.js Defense-in-Depth Implementation](./typescript_defense_in_depth.ts)

## Security Event Response Plan

In addition to preventive measures, establish a response plan for JWT-related security events:

1. **Detection**: Identify compromise indicators
2. **Containment**: Revoke affected tokens and mitigate ongoing attacks
3. **Eradication**: Remove vulnerability or exploitation path
4. **Recovery**: Restore normal operation with enhanced security
5. **Lessons Learned**: Update security controls based on incident

## Conclusion

Effective JWT security requires multiple defensive layers that work together. By implementing these defense-in-depth strategies, applications can continue to operate securely even if one security control is compromised or bypassed.