-- | Error types for Habitica API operations.
module ExoMonad.Habitica.Error
  ( HabiticaError (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Structured error type for Habitica API failures.
data HabiticaError
  = -- | "Your session is outdated" - need re-auth
    HabiticaSessionExpired
  | -- | Rate limit hit, retry later
    HabiticaRateLimited
  | -- | Task/item not found (includes ID)
    HabiticaNotFound Text
  | -- | Invalid credentials
    HabiticaUnauthorized
  | -- | Other errors with message
    HabiticaOther Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
