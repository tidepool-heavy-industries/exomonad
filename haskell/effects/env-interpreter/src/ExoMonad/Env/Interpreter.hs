{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
-- | Env effect interpreter.
module ExoMonad.Env.Interpreter
  ( runEnvIO
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import qualified Data.Text as T
import System.Environment (lookupEnv, getEnvironment)

import ExoMonad.Effects.Env (Env(..))

-- | Run Env effects using standard Haskell environment operations.
runEnvIO :: LastMember IO effs => Eff (Env ': effs) a -> Eff effs a
runEnvIO = interpret $ \case
  GetEnv name -> sendM $ fmap (fmap T.pack) (lookupEnv (T.unpack name))
  GetEnvironment -> sendM $ fmap (map (\(k, v) -> (T.pack k, T.pack v))) getEnvironment
