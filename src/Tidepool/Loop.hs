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

import Tidepool.Effect (State, LLM, TurnResult, TurnOutcome(..))
import qualified Tidepool.Effect as E
import Tidepool.Template
import Tidepool.Tool (toolListToJSON)
import Effectful (Effect, Eff, (:>))
import Data.Aeson (ToJSON, FromJSON)

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

-- | Execute a turn using configuration
-- Gets state, builds context, runs LLM turn, applies output on completion
-- Returns TurnOutcome to allow caller to handle breaks (state transitions)
--
-- NOTE: This generic abstraction is currently unused. The DM game uses
-- DM.Loop directly which builds system prompt from mood+context.
-- TODO: Update to use new runTurn API with systemPrompt + userAction
executeTurnConfig
  :: ( State state :> es
     , LLM :> es
     , GingerContext context
     , ToJSON context
     , FromJSON output
     )
  => TurnConfig state context output event extraEs tools
  -> Eff es (TurnOutcome (TurnResult output))
executeTurnConfig _config = error "TODO: executeTurnConfig needs update for new runTurn API"

-- | Check if compression is needed
shouldCompress 
  :: [history]
  -> Int  -- threshold
  -> Bool
shouldCompress history threshold = length history > threshold

-- | Run compression on history
-- Returns TurnOutcome (compression shouldn't break, but we handle it)
-- TODO: Update to use new runTurn API with systemPrompt + userAction
compress
  :: ( State state :> es
     , LLM :> es
     , GingerContext compressionContext
     , ToJSON compressionContext
     , FromJSON compressionOutput
     )
  => Compression state history compressionContext compressionOutput event extraEs tools
  -> [history]
  -> Eff es (TurnOutcome ())
compress _comp _history = error "TODO: compress needs update for new runTurn API"

-- | Build context from state (helper)
buildContext :: (state -> context) -> state -> context
buildContext f s = f s
