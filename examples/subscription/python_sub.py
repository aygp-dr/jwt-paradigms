#!/usr/bin/env python3

import jwt
import time
import os
from datetime import datetime, timedelta

SECRET_KEY = os.environ.get('JWT_SECRET', 'your-secret-key')

def generate_subscription_token(user_id, plan_details):
    """Generate a JWT token containing subscription information"""
    now = int(time.time())
    expiration = now + (plan_details['duration_days'] * 86400)  # Convert days to seconds
    
    payload = {
        'sub': user_id,
        'iat': now,
        'exp': expiration,
        'plan': {
            'name': plan_details['name'],
            'tier': plan_details['tier'],
            'features': plan_details['features'],
            'is_active': True
        },
        'meta': {
            'last_billing_date': plan_details['last_billing_date'],
            'next_billing_date': plan_details['next_billing_date']
        }
    }
    
    return jwt.encode(payload, SECRET_KEY, algorithm='HS256')

def verify_subscription_access(token):
    """Verify a subscription token and check if the user has access"""
    try:
        decoded = jwt.decode(token, SECRET_KEY, algorithms=['HS256'])
        
        # Check if token contains subscription plan
        if not decoded.get('plan') or not decoded['plan'].get('is_active'):
            return {'valid': False, 'reason': 'No active subscription found'}
        
        # Return subscription details
        return {
            'valid': True,
            'user_id': decoded['sub'],
            'plan': decoded['plan']['name'],
            'tier': decoded['plan']['tier'],
            'features': decoded['plan']['features'],
            'expires_at': datetime.fromtimestamp(decoded['exp']).isoformat()
        }
    except jwt.ExpiredSignatureError:
        return {'valid': False, 'reason': 'Subscription expired'}
    except (jwt.InvalidTokenError, KeyError):
        return {'valid': False, 'reason': 'Invalid token'}

# Example usage
if __name__ == "__main__":
    plan_details = {
        'name': 'Premium',
        'tier': 2,
        'features': ['content-access', 'hd-streaming', 'offline-download'],
        'duration_days': 30,
        'last_billing_date': '2025-04-01',
        'next_billing_date': '2025-05-01'
    }
    
    token = generate_subscription_token('user123', plan_details)
    print(f"Subscription Token: {token}")
    
    verification = verify_subscription_access(token)
    print(f"Verification Result: {verification}")
