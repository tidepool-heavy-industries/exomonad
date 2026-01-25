{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Effects.Effector
  ( -- * Effect
    Effector(..)
  , runEffector

    -- * Types
  , GhPrStatusResult(..)
  , GhPrCreateResult(..)
  , PrComment(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Result of @effector gh pr-status@
data GhPrStatusResult = GhPrStatusResult
  { exists :: Bool
  , url :: Maybe Text
  , number :: Maybe Int
  , state :: Maybe Text
  , review_status :: Maybe Text
  , comments :: [PrComment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comment on a PR, potentially inline
data PrComment = PrComment
  { author :: Text
  , body :: Text
  , path :: Maybe Text
  , line :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of @effector gh pr-create@
data GhPrCreateResult = GhPrCreateResult
  { url :: Text
  , number :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Effector effect for running the @effector@ tool.
--
-- This tool handles SSH transparently for subagent control.
data Effector r where
  RunEffector :: Text -> [Text] -> Effector Text

-- | Run the @effector@ tool with the given command and arguments.
-- Returns the raw stdout as Text.
runEffector :: Member Effector effs => Text -> [Text] -> Eff (effs) Text
runEffector cmd args = send (RunEffector cmd args)
