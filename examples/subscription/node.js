const jwt = require('jsonwebtoken');
const SECRET_KEY = process.env.JWT_SECRET || 'your-secret-key';

// Generate a subscription token
function generateSubscriptionToken(userId, planDetails) {
  const now = Math.floor(Date.now() / 1000);
  const payload = {
    sub: userId,
    iat: now,
    exp: now + (planDetails.durationDays * 86400), // Convert days to seconds
    plan: {
      name: planDetails.name,
      tier: planDetails.tier,
      features: planDetails.features,
      isActive: true
    },
    meta: {
      lastBillingDate: planDetails.lastBillingDate,
      nextBillingDate: planDetails.nextBillingDate
    }
  };
  
  return jwt.sign(payload, SECRET_KEY, { algorithm: 'HS256' });
}

// Verify a subscription token
function verifySubscriptionAccess(token) {
  try {
    const decoded = jwt.verify(token, SECRET_KEY);
    
    // Check if token contains subscription plan
    if (!decoded.plan || !decoded.plan.isActive) {
      return { valid: false, reason: 'No active subscription found' };
    }
    
    // Check for required features
    return {
      valid: true,
      userId: decoded.sub,
      plan: decoded.plan.name,
      tier: decoded.plan.tier,
      features: decoded.plan.features,
      expiresAt: new Date(decoded.exp * 1000)
    };
  } catch (error) {
    if (error.name === 'TokenExpiredError') {
      return { valid: false, reason: 'Subscription expired' };
    }
    return { valid: false, reason: 'Invalid token' };
  }
}

// Example usage
const planDetails = {
  name: 'Premium',
  tier: 2,
  features: ['content-access', 'hd-streaming', 'offline-download'],
  durationDays: 30,
  lastBillingDate: '2025-04-01',
  nextBillingDate: '2025-05-01'
};

const token = generateSubscriptionToken('user123', planDetails);
console.log('Subscription Token:', token);

const verification = verifySubscriptionAccess(token);
console.log('Verification Result:', verification);
