{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core types for the agent graph.
--
-- This module uses FieldSelectors to enable TH introspection of record fields.
-- Types here are compiled before Template/Graph.hs to satisfy TH staging.
module Template.Types
  ( Input(..)
  , Output(..)
  , Result(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Input type - what enters the graph.
newtype Input = Input
  { inputText :: String
    -- ^ The input text to process
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Output type - what the LLM produces (structured output).
newtype Output = Output
  { outputText :: String
    -- ^The processed output text from the LLM
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result type - what exits the graph.
newtype Result = Result
  { resultText :: String
    -- ^ The final result text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
