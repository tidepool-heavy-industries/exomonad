-- | Habitica effect executor - native HTTP client implementation.
--
-- Implements Habitica effect using http-client-tls.
module Tidepool.Habitica.Executor
  ( -- * Executor
    runHabitica
  , HabiticaConfig(..)
  ) where

import Data.Text (Text)

-- | Habitica API configuration.
data HabiticaConfig = HabiticaConfig
  { hcBaseUrl :: Text
  , hcUserId :: Text
  , hcApiToken :: Text
  }

-- | Run Habitica effects (stub - actual implementation by Agent 5).
runHabitica :: HabiticaConfig -> a -> a
runHabitica _ = id  -- stub
