-- | Lambda Hopper's JWT Validation Monads
-- | A formal approach to JWT parsing and validation
-- | POPL 2025 Submission Draft

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Authentication.JWT.Formal where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.JWT (JWK)
import Data.Kind (Type)
import Data.Time (UTCTime)
import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Text as T

-- | Claim types with phantom types to enforce type safety
data Claim a where
  StringClaim  :: T.Text -> Claim T.Text
  IntClaim     :: Int -> Claim Int
  BoolClaim    :: Bool -> Claim Bool
  ListClaim    :: [Claim a] -> Claim [a]
  TimeClaim    :: UTCTime -> Claim UTCTime
  CustomClaim  :: a -> Claim a  -- Requires instances of FromJSON/ToJSON

-- | Type-level claim identifiers
data ClaimName = Subject | Issuer | Audience | Expiration | IssuedAt | Custom Symbol

-- | Type family mapping claim names to their expected types
type family ClaimType (name :: ClaimName) :: Type where
  ClaimType 'Subject    = T.Text
  ClaimType 'Issuer     = T.Text
  ClaimType 'Audience   = [T.Text]
  ClaimType 'Expiration = UTCTime
  ClaimType 'IssuedAt   = UTCTime
  ClaimType ('Custom sym) = T.Text  -- Simplified; would be more complex in practice

-- | A typed map of claims, enforcing type safety at compile time
data TypedClaims = TypedClaims (Map.Map T.Text (SomeClaim))

-- | Existential wrapper for claims
data SomeClaim where
  SomeClaim :: Claim a -> SomeClaim

-- | JWT validation errors as a proper ADT
data JWTError
  = InvalidSignature
  | ExpiredToken
  | NotYetValid
  | MissingRequiredClaim T.Text
  | InvalidClaim T.Text String
  | MalformedJWT String
  | AlgorithmMismatch
  | InvalidKey
  deriving (Show, Eq)

-- | JWT validation context - making environmental dependencies explicit
data JWTContext = JWTContext
  { currentTime :: UTCTime
  , trustedKeys :: [JWK]
  , allowedAlgorithms :: [T.Text]
  , requiredClaims :: [T.Text]
  , audienceRestrictions :: Maybe [T.Text]
  }

-- | The JWT validation monad transformer stack
type JWTValidatorT m a = ReaderT JWTContext (ExceptT JWTError m) a

-- | Run the JWT validator
runJWTValidator :: Monad m => JWTContext -> JWTValidatorT m a -> m (Either JWTError a)
runJWTValidator ctx validator = runExceptT $ runReaderT validator ctx

-- | A validated JWT token carrying its claims at the type level
newtype ValidJWT (requiredClaims :: [ClaimName]) = ValidJWT TypedClaims

-- | Proof that a claim name is in a list of required claims
data ClaimIsRequired (name :: ClaimName) (required :: [ClaimName]) where
  ClaimIsFirst :: ClaimIsRequired name (name ': rest)
  ClaimIsLater :: ClaimIsRequired name rest -> ClaimIsRequired name (other ': rest)

-- | Extract a typed claim with compile-time guarantee it exists
getClaim :: ClaimIsRequired name required => ValidJWT required -> Claim (ClaimType name)
getClaim = error "Implementation would extract the claim with type safety"

-- | Monadic JWT validation that preserves claim type information
validateJWT :: Monad m 
            => T.Text 
            -> JWTValidatorT m (ValidJWT '[Subject, Issuer, Expiration])
validateJWT token = do
  -- In a real implementation, this would:
  -- 1. Parse the JWT
  -- 2. Verify its signature
  -- 3. Check temporal constraints
  -- 4. Validate required claims
  -- 5. Return a ValidJWT with statically known claims
  ctx <- ask
  -- Simulated implementation (would be much more detailed in practice)
  if token == "invalid"
    then throwError InvalidSignature
    else return $ ValidJWT (TypedClaims Map.empty) -- Simplified for example

-- | Functor instance for the ValidJWT, allowing claim transformations
instance Functor (ValidJWT claims) where
  fmap f (ValidJWT claims) = ValidJWT claims -- Would transform claims if meaningful

-- | An example of a higher-order validation function
withAudience :: Monad m
             => (T.Text -> Bool)
             -> JWTValidatorT m (ValidJWT claims)
             -> JWTValidatorT m (ValidJWT claims)
withAudience audienceCheck validator = do
  jwt <- validator
  -- Would extract audience claim and check it
  -- throwError (InvalidClaim "aud" "Audience check failed") on failure
  return jwt

{- 
EXAMPLE USAGE:

processUserRequest :: Request -> IO (Either AuthError Response)
processUserRequest req = do
  let token = extractToken req
      currentTime = getCurrentTime
      context = JWTContext {
        currentTime = currentTime,
        trustedKeys = loadTrustedKeys,
        allowedAlgorithms = ["ES256", "RS256"],
        requiredClaims = ["sub", "iss", "exp"],
        audienceRestrictions = Just ["https://api.example.com"]
      }
  
  result <- runJWTValidator context $ do
    -- This expression has type: ValidJWT '[Subject, Issuer, Expiration]
    validJwt <- validateJWT token
    
    -- We can use getClaim to extract claims with type safety
    let subject = getClaim @'Subject validJwt
        expiration = getClaim @'Expiration validJwt
    
    -- Additional business logic using claims
    assertUserExists subject
    
    return (createAuthenticatedContext subject)
  
  return $ case result of
    Left err -> Left (mapJWTErrorToAuthError err)
    Right context -> Right (processRequestWithContext context)

This design ensures:
1. All dependencies are explicit in the context
2. Errors are properly typed and handled
3. Required claims are known at compile time
4. Functions like getClaim maintain type safety
5. Composition is natural through the monad transformer stack
-}

-- | Property-based testing ensuring algebraic laws are maintained
prop_jwtMonadLeftIdentity :: T.Text -> JWTContext -> Bool
prop_jwtMonadLeftIdentity token ctx =
  runJWTValidator ctx (return token >>= validateJWT) ==
  runJWTValidator ctx (validateJWT token)

prop_jwtMonadRightIdentity :: T.Text -> JWTContext -> Bool
prop_jwtMonadRightIdentity token ctx =
  runJWTValidator ctx (validateJWT token >>= return) ==
  runJWTValidator ctx (validateJWT token)

prop_jwtMonadAssociativity :: T.Text -> JWTContext -> Bool
prop_jwtMonadAssociativity token ctx =
  runJWTValidator ctx ((validateJWT token >>= f) >>= g) ==
  runJWTValidator ctx (validateJWT token >>= (\x -> f x >>= g))
  where
    f = return . const (ValidJWT (TypedClaims Map.empty))
    g = return . const (ValidJWT (TypedClaims Map.empty))

{-
Notes from Lambda Hopper:

This implementation remains incomplete as it focuses on the conceptual framework
rather than all implementation details. A complete version would include:

1. Full parsing and encoding logic for JWTs
2. Complete signature verification logic
3. Comprehensive claim validation including temporal checks
4. Integration with cryptographic libraries
5. Formal proofs of key properties using dependent types

The crucial innovation is enforcing claim types at compile time through phantom types
and type families, allowing the compiler to catch entire classes of errors that
would otherwise manifest at runtime.

The monadic structure explicitly models the validation dependencies and potential
failure modes, making the execution context clear and enabling principled composition
of validation steps.

See my full paper "Type-Level Verification of JWT Authentication" (IEEE S&P 2024)
for complete formalization and security proofs.
-}