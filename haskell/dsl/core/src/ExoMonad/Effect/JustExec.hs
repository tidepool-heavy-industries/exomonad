{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Effect.JustExec
  ( -- * Effect
    JustExec(..)
  , runRecipe

    -- * Types
  , ExecResult(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.:?), object, (.=))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

-- | Result of running a recipe execution.
data ExecResult = ExecResult
  { exitCode :: Int
  , stdout   :: Text
  , stderr   :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExecResult where
  parseJSON = withObject "ExecResult" $ \v -> ExecResult
    <$> (fromMaybe (-1) <$> v .:? "exit_code")
    <*> v .: "stdout"
    <*> v .: "stderr"

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
