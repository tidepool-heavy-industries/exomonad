-- | Env effect for reading environment variables.
module ExoMonad.Effects.Env
  ( Env (..),
    getEnv,
    lookupEnv,
    getEnvironment,
  )
where

import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)

-- | Env effect for reading environment variables.
data Env r where
  GetEnv :: Text -> Env (Maybe Text)
  GetEnvironment :: Env [(Text, Text)]

-- | Get an environment variable.
getEnv :: (Member Env effs) => Text -> Eff effs (Maybe Text)
getEnv = send . GetEnv

-- | Lookup an environment variable (alias for getEnv).
lookupEnv :: (Member Env effs) => Text -> Eff effs (Maybe Text)
lookupEnv = send . GetEnv

-- | Get all environment variables.
getEnvironment :: (Member Env effs) => Eff effs [(Text, Text)]
getEnvironment = send GetEnvironment
