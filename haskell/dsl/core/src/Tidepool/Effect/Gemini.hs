{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Gemini effect for running prompts via gemini CLI.
module Tidepool.Effect.Gemini
  ( -- * Effect
    GeminiOp(..)
  , GeminiModel(..)
  , GeminiResult(..)
  , runGemini
  
    -- * Singletons
  , SGeminiModel(..)
  , SingGeminiModel(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.NodeMeta (NodeMeta, NodeMetadata, getNodeMeta)
import Tidepool.Platform (NativeOnly)

-- | Gemini models available in the CLI.
data GeminiModel = Flash | Pro | Ultra
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Singleton for GeminiModel.
data SGeminiModel (model :: GeminiModel) where
  SFlash :: SGeminiModel 'Flash
  SPro   :: SGeminiModel 'Pro
  SUltra :: SGeminiModel 'Ultra

-- | Demote type-level GeminiModel to runtime singleton.
class SingGeminiModel (model :: GeminiModel) where
  singGeminiModel :: SGeminiModel model

instance SingGeminiModel 'Flash where singGeminiModel = SFlash
instance SingGeminiModel 'Pro where singGeminiModel = SPro
instance SingGeminiModel 'Ultra where singGeminiModel = SUltra

-- | Result of a Gemini CLI run.
data GeminiResult = GeminiResult
  { grOutput :: Value
  , grRawResponse :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Gemini effect.
data GeminiOp r where
  RunGemini :: NodeMetadata -> GeminiModel -> Text -> GeminiOp GeminiResult

-- | Run a Gemini prompt.
runGemini :: (Member GeminiOp effs, Member NodeMeta effs, NativeOnly) => GeminiModel -> Text -> Eff effs GeminiResult
runGemini model prompt = do
  meta <- getNodeMeta
  send (RunGemini meta model prompt)
