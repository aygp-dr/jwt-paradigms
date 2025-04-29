import com.auth0.jwt.JWT;
import com.auth0.jwt.JWTVerifier;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;

import java.util.Date;

public class JwtValidator {

    private final String issuer;
    private final String audience;
    private final Algorithm algorithm;

    /**
     * Creates a JWT validator with proper security checks
     *
     * @param secret The secret key used to sign the token
     * @param issuer Expected issuer of the token
     * @param audience Expected audience of the token
     */
    public JwtValidator(String secret, String issuer, String audience) {
        this.issuer = issuer;
        this.audience = audience;
        this.algorithm = Algorithm.HMAC256(secret);
    }

    /**
     * Properly validates a JWT token
     *
     * @param token The JWT token to validate
     * @return The decoded JWT if valid
     * @throws JWTVerificationException if token is invalid
     */
    public DecodedJWT validateToken(String token) throws JWTVerificationException {
        // Create a verifier with all required validations
        JWTVerifier verifier = JWT.require(algorithm)
                .withIssuer(issuer)             // Validate issuer
                .withAudience(audience)         // Validate audience
                .acceptLeeway(5)                // Small leeway for clock skew (5 seconds)
                .build();
        
        // This will throw if the token is invalid
        DecodedJWT jwt = verifier.verify(token);
        
        // Additional validation checks (example)
        validateAdditionalClaims(jwt);
        
        return jwt;
    }
    
    /**
     * Validates custom claims in the token
     */
    private void validateAdditionalClaims(DecodedJWT jwt) throws JWTVerificationException {
        // Example: Ensure token has a subject
        if (jwt.getSubject() == null || jwt.getSubject().isEmpty()) {
            throw new JWTVerificationException("Token missing required subject claim");
        }
        
        // Example: Custom validation based on your requirements
        // e.g., validate permissions, role, or any custom claims
        String role = jwt.getClaim("role").asString();
        if (role == null) {
            throw new JWTVerificationException("Token missing required role claim");
        }
    }

    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        try {
            // Create a validator
            JwtValidator validator = new JwtValidator(
                    "your-secret-key", 
                    "your-auth-service",
                    "your-api"
            );
            
            // Sample token (in real code, this would come from a request)
            String token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...";
            
            // Validate the token (throws if invalid)
            DecodedJWT jwt = validator.validateToken(token);
            
            // Token is valid - you can now use the claims
            System.out.println("Token is valid!");
            System.out.println("Subject: " + jwt.getSubject());
            System.out.println("Expires at: " + jwt.getExpiresAt());
            System.out.println("Role: " + jwt.getClaim("role").asString());
            
        } catch (JWTVerificationException e) {
            System.err.println("Token validation failed: " + e.getMessage());
        }
    }
}
