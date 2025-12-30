{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Core game loop abstraction
-- NOTE: The DM game uses DM.Loop directly which builds system prompt from mood+context.
-- These types are kept for potential future generic abstraction.
module Tidepool.Loop
  ( -- * Loop Types
    TurnConfig(..)
  , Compression(..)

    -- * Loop Operations
  , shouldCompress

    -- * Context Building
  , buildContext
  ) where

import Tidepool.Template (Template)
import Effectful (Effect)

-- | Configuration for a single turn in the game loop
-- Captures how to build context, which template to use, and how to apply output
data TurnConfig state context output event (extraEs :: [Effect]) tools = TurnConfig
  { turnBuildContext :: state -> context
  , turnTemplate :: Template context output event state extraEs tools
  , turnApplyOutput :: output -> state -> state
  }

-- | Compression configuration
data Compression state history compressionContext compressionOutput event (extraEs :: [Effect]) tools = Compression
  { compressionThreshold :: Int
  , compressionBuildContext :: [history] -> state -> compressionContext
  , compressionTemplate :: Template compressionContext compressionOutput event state extraEs tools
  , compressionApply :: compressionOutput -> state -> state
  }

-- | Check if compression is needed
shouldCompress 
  :: [history]
  -> Int  -- threshold
  -> Bool
shouldCompress history threshold = length history > threshold

-- | Build context from state (helper)
buildContext :: (state -> context) -> state -> context
buildContext f s = f s
