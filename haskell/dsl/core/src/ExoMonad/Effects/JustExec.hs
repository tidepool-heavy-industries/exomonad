{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
    JustExec (..),
    execRecipe,

    -- * Types
    ExecResult (..),

    -- * Runner (stub)
    runJustExecStub,
  )
where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effect.Log (Log, logInfo)
import GHC.Generics (Generic)

-- | Result of running a recipe execution.
--
-- Fields are ordered to match JustResult: stdout, stderr, exitCode.
data ExecResult = ExecResult
  { stdout :: Text,
    stderr :: Text,
    exitCode :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v ->
    ExecResult
      <$> v .: "stdout"
      <*> v .: "stderr"
      <*> (fromMaybe (-1) <$> v .:? "exit_code")

instance ToJSON ExecResult where
  toJSON r =
    object
      [ "exit_code" .= r.exitCode,
        "stdout" .= r.stdout,
        "stderr" .= r.stderr
      ]

-- | JustExec effect for running recipes via docker-ctl.
data JustExec r where
  ExecRecipe :: Text -> [Text] -> JustExec ExecResult

-- | Run a recipe with arguments.
execRecipe :: (Member JustExec effs) => Text -> [Text] -> Eff effs ExecResult
execRecipe recipe args = send (ExecRecipe recipe args)

-- | Stub runner that logs calls and returns success with empty output.
runJustExecStub :: (Member Log effs) => Eff (JustExec ': effs) a -> Eff effs a
runJustExecStub = interpret $ \case
  ExecRecipe recipe args -> do
    logInfo $ "[JustExec:stub] ExecRecipe called: " <> recipe <> " " <> T.intercalate " " args
    pure $ ExecResult "" "" 0
