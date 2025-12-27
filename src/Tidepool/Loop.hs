{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Core game loop abstraction
module Tidepool.Loop
  ( -- * Loop Types
    TurnConfig(..)
  , Compression(..)

    -- * Loop Operations
  , executeTurnConfig
  , compress
  , shouldCompress

    -- * Context Building
  , buildContext
  ) where

import Tidepool.Effect (State, LLM, Emit, TurnResult)
import qualified Tidepool.Effect as E
import Tidepool.Template
import Effectful

-- | Configuration for a single turn in the game loop
-- Captures how to build context, which template to use, and how to apply output
data TurnConfig state context output event tools = TurnConfig
  { turnBuildContext :: state -> context
  , turnTemplate :: Template context output event tools
  , turnApplyOutput :: output -> state -> state
  }

-- | Compression configuration
data Compression state history compressionContext compressionOutput event tools = Compression
  { compressionThreshold :: Int
  , compressionBuildContext :: [history] -> state -> compressionContext
  , compressionTemplate :: Template compressionContext compressionOutput event tools
  , compressionApply :: compressionOutput -> state -> state
  }

-- | Execute a turn using configuration
-- Gets state, builds context, runs LLM turn, applies output
executeTurnConfig
  :: (State state :> es, LLM :> es, Emit event :> es)
  => TurnConfig state context output event tools
  -> input
  -> Eff es (TurnResult output)
executeTurnConfig _config _input = error "TODO: executeTurnConfig - get state, build context, call E.runTurn, apply to state"

-- | Check if compression is needed
shouldCompress 
  :: [history]
  -> Int  -- threshold
  -> Bool
shouldCompress history threshold = length history > threshold

-- | Run compression on history
compress
  :: (State state :> es, LLM :> es)
  => Compression state history compressionContext compressionOutput event tools
  -> [history]
  -> Eff es ()
compress _comp _history = error "TODO: compress - build compression context, call LLM, apply compression output"

-- | Build context from state (helper)
buildContext :: (state -> context) -> state -> context
buildContext f s = f s
