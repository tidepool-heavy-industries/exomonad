{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Env effect for reading environment variables.
module ExoMonad.Effects.Env
  ( Env (..),
    getEnv,
    lookupEnv,
    getEnvironment,
  )
where

import Data.Text (Text)
import Polysemy (Member, Sem, makeSem)
import Prelude hiding (lookupEnv)

-- | Env effect for reading environment variables.
data Env m a where
  GetEnv :: Text -> Env m (Maybe Text)
  GetEnvironment :: Env m [(Text, Text)]

makeSem ''Env

-- | Lookup an environment variable (alias for getEnv).
lookupEnv :: (Member Env r) => Text -> Sem r (Maybe Text)
lookupEnv = getEnv
