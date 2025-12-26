{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Core game loop abstraction
module Tidepool.Loop
  ( -- * Loop Types
    Turn(..)
  , TurnResult(..)
  , Compression(..)
  
    -- * Loop Operations
  , runTurn
  , compress
  , shouldCompress
  
    -- * Context Building
  , buildContext
  ) where

import Tidepool.Effect
import Tidepool.Template
import Effectful
import Data.Text (Text)

-- | A single turn in the game loop
data Turn state context output tools = Turn
  { turnBuildContext :: state -> context
  , turnTemplate :: Template context output tools
  , turnApplyOutput :: output -> state -> state
  }

-- | Result of running a turn
data TurnResult output = TurnResult
  { turnOutput :: output
  , turnResponse :: Text
  }
  deriving (Show, Eq)

-- | Compression configuration
data Compression state history compressionContext compressionOutput tools = Compression
  { compressionThreshold :: Int
  , compressionBuildContext :: [history] -> state -> compressionContext
  , compressionTemplate :: Template compressionContext compressionOutput tools
  , compressionApply :: compressionOutput -> state -> state
  }

-- | Run a single turn
runTurn
  :: (State state :> es, LLM :> es, Emit event :> es)
  => Turn state context output tools
  -> input
  -> Eff es (TurnResult output)
runTurn _turn _input = error "TODO: runTurn - get state, build context, render template, call LLM, parse output, apply to state"

-- | Check if compression is needed
shouldCompress 
  :: [history]
  -> Int  -- threshold
  -> Bool
shouldCompress history threshold = length history > threshold

-- | Run compression on history
compress
  :: (State state :> es, LLM :> es)
  => Compression state history compressionContext compressionOutput tools
  -> [history]
  -> Eff es ()
compress _comp _history = error "TODO: compress - build compression context, call LLM, apply compression output"

-- | Build context from state (helper)
buildContext :: (state -> context) -> state -> context
buildContext f s = f s
