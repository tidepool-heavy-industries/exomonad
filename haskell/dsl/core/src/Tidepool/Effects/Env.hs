-- | Env effect for reading environment variables.
module Tidepool.Effects.Env
  ( Env(..)
  , getEnv
  , lookupEnv
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)

-- | Env effect for reading environment variables.
data Env r where
  GetEnv :: Text -> Env (Maybe Text)

-- | Get an environment variable.
getEnv :: Member Env effs => Text -> Eff effs (Maybe Text)
getEnv = send . GetEnv

-- | Lookup an environment variable (alias for getEnv).
lookupEnv :: Member Env effs => Text -> Eff effs (Maybe Text)
lookupEnv = send . GetEnv
