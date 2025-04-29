import jwt
import secrets
import time
import uuid
import redis
import os
from datetime import datetime, timedelta

# Configuration
SECRET_KEY = os.environ.get('JWT_SECRET', 'your-secret-key')
REFRESH_SECRET = os.environ.get('REFRESH_SECRET', 'your-refresh-secret')

# Redis connection for token storage and revocation
redis_client = redis.Redis(
    host=os.environ.get('REDIS_HOST', 'localhost'),
    port=int(os.environ.get('REDIS_PORT', 6379)),
    db=int(os.environ.get('REDIS_DB', 0)),
    decode_responses=True
)

class TokenService:
    """Service for managing access and refresh tokens"""
    
    def __init__(self):
        self.access_ttl = 900  # 15 minutes
        self.refresh_ttl = 2592000  # 30 days
    
    def generate_token_pair(self, user_id, roles, permissions):
        """Generate a new access+refresh token pair"""
        now = int(time.time())
        
        # Create a unique refresh token ID
        refresh_jti = str(uuid.uuid4())
        
        # Access token payload
        access_payload = {
            'sub': user_id,
            'iat': now,
            'exp': now + self.access_ttl,
            'jti': str(uuid.uuid4()),
            'roles': roles,
            'permissions': permissions
        }
        
        # Refresh token payload
        refresh_payload = {
            'sub': user_id,
            'iat': now,
            'exp': now + self.refresh_ttl,
            'jti': refresh_jti,
            'type': 'refresh'
        }
        
        # Create tokens
        access_token = jwt.encode(access_payload, SECRET_KEY, algorithm='HS256')
        refresh_token = jwt.encode(refresh_payload, REFRESH_SECRET, algorithm='HS256')
        
        # Store refresh token in Redis for validation/revocation
        self._store_refresh_token(refresh_jti, user_id, now + self.refresh_ttl)
        
        return {
            'access_token': access_token,
            'token_type': 'Bearer',
            'expires_in': self.access_ttl,
            'refresh_token': refresh_token,
            'refresh_expires_in': self.refresh_ttl
        }
    
    def refresh_access_token(self, refresh_token):
        """Use a refresh token to generate a new access token"""
        try:
            # Verify the refresh token
            decoded = jwt.decode(refresh_token, REFRESH_SECRET, algorithms=['HS256'])
            
            # Check if it's a refresh token
            if decoded.get('type') != 'refresh':
                return {'error': 'Invalid token type'}
            
            # Check if token has been revoked
            if not self._validate_refresh_token(decoded['jti']):
                return {'error': 'Token has been revoked'}
            
            # Get user data (in a real app, you'd get the latest roles/permissions)
            user_id = decoded['sub']
            user_data = self._get_user_data(user_id)
            
            # Generate a new access token
            now = int(time.time())
            new_access_payload = {
                'sub': user_id,
                'iat': now,
                'exp': now + self.access_ttl,
                'jti': str(uuid.uuid4()),
                'roles': user_data['roles'],
                'permissions': user_data['permissions']
            }
            
            new_access_token = jwt.encode(new_access_payload, SECRET_KEY, algorithm='HS256')
            
            return {
                'access_token': new_access_token,
                'token_type': 'Bearer',
                'expires_in': self.access_ttl
            }
            
        except jwt.ExpiredSignatureError:
            return {'error': 'Refresh token expired'}
        except jwt.InvalidTokenError:
            return {'error': 'Invalid token'}
    
    def revoke_refresh_token(self, refresh_token):
        """Revoke a refresh token"""
        try:
            decoded = jwt.decode(refresh_token, REFRESH_SECRET, algorithms=['HS256'])
            self._revoke_refresh_token(decoded['jti'])
            return {'success': True, 'message': 'Token revoked'}
        except (jwt.InvalidTokenError, KeyError):
            return {'error': 'Invalid token'}
    
    def _store_refresh_token(self, jti, user_id, expiry):
        """Store refresh token metadata in Redis"""
        # Key format: refresh_token:{jti}
        key = f"refresh_token:{jti}"
        redis_client.hset(key, mapping={
            'user_id': user_id,
            'created_at': int(time.time()),
            'revoked': 'false'
        })
        # Set expiration
        redis_client.expireat(key, expiry)
    
    def _validate_refresh_token(self, jti):
        """Check if a refresh token is valid and not revoked"""
        key = f"refresh_token:{jti}"
        # Check if token exists and is not revoked
        token_data = redis_client.hgetall(key)
        return token_data and token_data.get('revoked') == 'false'
    
    def _revoke_refresh_token(self, jti):
        """Mark a refresh token as revoked"""
        key = f"refresh_token:{jti}"
        redis_client.hset(key, 'revoked', 'true')
    
    def _get_user_data(self, user_id):
        """Get latest user data (roles/permissions)"""
        # In a real app, you would fetch this from your database
        # This is a mockup for demonstration
        return {
            'roles': ['user', 'subscriber'],
            'permissions': ['read:content', 'post:comments']
        }

# Example usage
if __name__ == "__main__":
    token_service = TokenService()
    
    # Generate token pair for a user
    user_id = 'user789'
    roles = ['user', 'subscriber']
    permissions = ['read:content', 'post:comments']
    
    token_pair = token_service.generate_token_pair(user_id, roles, permissions)
    print(f"Token Pair: {token_pair}\n")
    
    # Simulate using the refresh token to get a new access token
    refresh_result = token_service.refresh_access_token(token_pair['refresh_token'])
    print(f"Refresh Result: {refresh_result}\n")
    
    # Revoke the refresh token
    revoke_result = token_service.revoke_refresh_token(token_pair['refresh_token'])
    print(f"Revoke Result: {revoke_result}\n")
    
    # Try to use the revoked refresh token
    failed_refresh = token_service.refresh_access_token(token_pair['refresh_token'])
    print(f"Using Revoked Token: {failed_refresh}")
