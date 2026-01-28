{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | JustExec effect for structured recipe execution.
--
-- This effect expects the underlying runner (e.g., docker-ctl) to return JSON output
-- in the following format:
--
-- > {
-- >   "exit_code": <int|null>,
-- >   "stdout": "<string>",
-- >   "stderr": "<string>"
-- > }
--
-- A null `exit_code` defaults to -1.
module ExoMonad.Effects.JustExec
  ( -- * Effect
    JustExec(..)
  , runRecipe

    -- * Types
  , ExecResult(..)

    -- * Runner (stub)
  , runJustExecStub
  ) where

import Control.Monad.Freer (Eff, Member, send, interpret)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.:?), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import ExoMonad.Effect.Log (Log, logInfo)

-- | Result of running a recipe execution.
--
-- Fields are ordered to match JustResult: stdout, stderr, exitCode.
data ExecResult = ExecResult
  { stdout   :: Text
  , stderr   :: Text
  , exitCode :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v -> ExecResult
    <$> v .: "stdout"
    <*> v .: "stderr"
    <*> (fromMaybe (-1) <$> v .:? "exit_code")

instance ToJSON ExecResult where
  toJSON r = object
    [ "exit_code" .= r.exitCode
    , "stdout" .= r.stdout
    , "stderr" .= r.stderr
    ]

-- | JustExec effect for running recipes via docker-ctl.
data JustExec r where
  RunRecipe :: Text -> [Text] -> JustExec ExecResult

-- | Run a recipe with arguments.
runRecipe :: Member JustExec effs => Text -> [Text] -> Eff effs ExecResult
runRecipe recipe args = send (RunRecipe recipe args)

-- | Stub runner that logs calls and returns success with empty output.
runJustExecStub :: Member Log effs => Eff (JustExec ': effs) a -> Eff effs a
runJustExecStub = interpret $ \case
  RunRecipe recipe args -> do
    logInfo $ "[JustExec:stub] RunRecipe called: " <> recipe <> " " <> T.intercalate " " args
    pure $ ExecResult "" "" 0