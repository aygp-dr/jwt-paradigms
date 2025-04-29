require 'jwt'
require 'redis'
require 'securerandom'
require 'digest'

# Configuration
SECRET_KEY = ENV['JWT_SECRET'] || 'your-secret-key'
REDIS_URL = ENV['REDIS_URL'] || 'redis://localhost:6379/0'
ACCESS_TTL = 15 * 60 # 15 minutes in seconds
REVOKE_PREFIX = 'revoked_token:'

class RevocationService
  def initialize
    @redis = Redis.new(url: REDIS_URL)
  end

  # Check if a token is revoked
  def token_revoked?(token_id)
    key = "#{REVOKE_PREFIX}#{token_id}"
    @redis.exists?(key) == 1
  end

  # Revoke a token by its ID
  def revoke_token(token_id, expires_at)
    key = "#{REVOKE_PREFIX}#{token_id}"
    ttl = expires_at - Time.now.to_i
    return if ttl <= 0 # Already expired, no need to revoke

    @redis.set(key, 1, ex: ttl)
  end

  # Revoke all tokens in a family
  def revoke_token_family(family, until_time = nil)
    until_time ||= Time.now.to_i + (30 * 24 * 60 * 60) # Default to 30 days
    key = "#{REVOKE_PREFIX}family:#{family}"
    ttl = until_time - Time.now.to_i
    ttl = 30 * 24 * 60 * 60 if ttl <= 0 # Default to 30 days if until_time is in the past

    @redis.set(key, Time.now.to_i, ex: ttl)
  end

  # Check if a token family is revoked
  def token_family_revoked?(family, issued_at)
    key = "#{REVOKE_PREFIX}family:#{family}"
    revoked_at = @redis.get(key)
    return false unless revoked_at

    issued_at < revoked_at.to_i
  end
end

class TokenService
  def initialize
    @revocation_service = RevocationService.new
  end

  # Create a new JWT token
  def create_token(user_id, roles, permissions)
    now = Time.now.to_i
    expires_at = now + ACCESS_TTL
    token_id = SecureRandom.uuid
    
    # Generate a family identifier for related tokens
    family = generate_token_family(user_id)

    payload = {
      sub: user_id,
      exp: expires_at,
      iat: now,
      nbf: now,
      jti: token_id,
      roles: roles,
      permissions: permissions,
      fam: family,
      meta: {
        ip: '192.168.1.100', # Would be actual client IP in real app
      }
    }

    JWT.encode(payload, SECRET_KEY, 'HS256')
  end

  # Verify and parse a token
  def verify_token(token_string)
    begin
      decoded_token = JWT.decode(token_string, SECRET_KEY, true, { algorithm: 'HS256' })
      payload = decoded_token[0]
      token_id = payload['jti']

      # Check if token is revoked
      if @revocation_service.token_revoked?(token_id)
        raise JWT::VerificationError, 'Token is revoked'
      end

      # Check if token family is revoked
      family = payload['fam']
      if family && @revocation_service.token_family_revoked?(family, payload['iat'])
        raise JWT::VerificationError, 'Token family is revoked'
      end

      payload
    rescue JWT::ExpiredSignature
      raise 'Token has expired'
    rescue JWT::DecodeError, JWT::VerificationError => e
      raise "Invalid token: #{e.message}"
    end
  end

  # Revoke a specific token
  def revoke_token(token_string)
    begin
      decoded_token = JWT.decode(token_string, SECRET_KEY, true, { algorithm: 'HS256' })
      payload = decoded_token[0]
      token_id = payload['jti']
      expires_at = payload['exp']

      @revocation_service.revoke_token(token_id, expires_at)
    rescue JWT::DecodeError => e
      raise "Failed to parse token: #{e.message}"
    end
  end

  # Revoke all tokens for a user
  def revoke_all_user_tokens(user_id)
    family = generate_token_family(user_id)
    # Revoke for 30 days (or longer if needed)
    @revocation_service.revoke_token_family(family)
  end

  private

  # Generate a unique family identifier for a user
  def generate_token_family(user_id)
    Digest::SHA256.hexdigest(user_id)
  end
end

# Example usage
if __FILE__ == $0
  token_service = TokenService.new

  # Create a token
  user_id = 'user123'
  roles = ['user', 'admin']
  permissions = ['read:users', 'write:users']

  token = token_service.create_token(user_id, roles, permissions)
  puts "Token created: #{token}\n\n"

  # Verify the token
  begin
    claims = token_service.verify_token(token)
    puts "Token verified. User: #{claims['sub']}, Roles: #{claims['roles']}\n\n"
  rescue => e
    puts "Failed to verify token: #{e.message}"
  end

  # Revoke the token
  begin
    token_service.revoke_token(token)
    puts "Token revoked"
  rescue => e
    puts "Failed to revoke token: #{e.message}"
  end

  # Try to verify the revoked token
  begin
    token_service.verify_token(token)
  rescue => e
    puts "Verification after revocation: #{e.message}\n\n"
  end

  # Revoke all user tokens
  begin
    token_service.revoke_all_user_tokens(user_id)
    puts "All user tokens revoked"
  rescue => e
    puts "Failed to revoke all user tokens: #{e.message}"
  end

  # Create a new token and try to verify (should fail due to family revocation)
  new_token = token_service.create_token(user_id, roles, permissions)
  begin
    token_service.verify_token(new_token)
  rescue => e
    puts "Verification of new token after family revocation: #{e.message}"
  end
end
