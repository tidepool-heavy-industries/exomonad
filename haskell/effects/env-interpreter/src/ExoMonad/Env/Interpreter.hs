{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Env effect interpreter.
module ExoMonad.Env.Interpreter
  ( runEnvIO,
  )
where

import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Embed (Embed)
import Data.Text qualified as T
import ExoMonad.Effects.Env (Env (..))
import System.Environment (getEnvironment, lookupEnv)

-- | Run Env effects using standard Haskell environment operations.
runEnvIO :: (Member (Embed IO) r) => Sem (Env ': r) a -> Sem r a
runEnvIO = interpret $ \case
  GetEnv name -> embed $ fmap (fmap T.pack) (lookupEnv (T.unpack name))
  GetEnvironment -> embed $ fmap (map (\(k, v) -> (T.pack k, T.pack v))) getEnvironment
