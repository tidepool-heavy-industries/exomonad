-- | Error types for Habitica API operations.
module Tidepool.Habitica.Error
  ( HabiticaError(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Structured error type for Habitica API failures.
data HabiticaError
  = HabiticaSessionExpired    -- ^ "Your session is outdated" - need re-auth
  | HabiticaRateLimited       -- ^ Rate limit hit, retry later
  | HabiticaNotFound Text     -- ^ Task/item not found (includes ID)
  | HabiticaUnauthorized      -- ^ Invalid credentials
  | HabiticaOther Text        -- ^ Other errors with message
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
