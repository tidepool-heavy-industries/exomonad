{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Env effect for reading environment variables.
module ExoMonad.Effects.Env
  ( Env (..),
    getEnv,
    lookupEnv,
    getEnvironment,
  )
where

import Prelude hiding (lookupEnv)

import Polysemy (Sem, Member, makeSem)
import Data.Text (Text)

-- | Env effect for reading environment variables.
data Env m a where
  GetEnv :: Text -> Env m (Maybe Text)
  GetEnvironment :: Env m [(Text, Text)]

makeSem ''Env

-- | Lookup an environment variable (alias for getEnv).
lookupEnv :: (Member Env r) => Text -> Sem r (Maybe Text)
lookupEnv = getEnv

