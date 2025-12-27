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
import Effectful
import Data.Aeson (ToJSON, FromJSON)

-- | Configuration for a single turn in the game loop
-- Captures how to build context, which template to use, and how to apply output
data TurnConfig state context output event tools = TurnConfig
  { turnBuildContext :: state -> context
  , turnTemplate :: Template context output event state tools
  , turnApplyOutput :: output -> state -> state
  }

-- | Compression configuration
data Compression state history compressionContext compressionOutput event tools = Compression
  { compressionThreshold :: Int
  , compressionBuildContext :: [history] -> state -> compressionContext
  , compressionTemplate :: Template compressionContext compressionOutput event state tools
  , compressionApply :: compressionOutput -> state -> state
  }

-- | Execute a turn using configuration
-- Gets state, builds context, runs LLM turn, applies output on completion
-- Returns TurnOutcome to allow caller to handle breaks (state transitions)
executeTurnConfig
  :: ( State state :> es
     , LLM :> es
     , GingerContext context
     , ToJSON context
     , FromJSON output
     )
  => TurnConfig state context output event tools
  -> Eff es (TurnOutcome (TurnResult output))
executeTurnConfig config = do
  -- 1. Get current state
  state <- E.get

  -- 2. Build context from state
  let context = config.turnBuildContext state

  -- 3. Extract template components
  let template = config.turnTemplate
      renderFn = render template
      schema = template.templateOutputSchema.schemaJSON
      tools = toolListToJSON template.templateTools

  -- 4. Call LLM with rendered template
  outcome <- E.runTurn renderFn schema tools context

  -- 5. Only apply output if turn completed (not broken)
  case outcome of
    TurnCompleted result -> do
      E.modify (config.turnApplyOutput result.trOutput)
      return (TurnCompleted result)
    TurnBroken reason ->
      return (TurnBroken reason)

-- | Check if compression is needed
shouldCompress 
  :: [history]
  -> Int  -- threshold
  -> Bool
shouldCompress history threshold = length history > threshold

-- | Run compression on history
-- Returns TurnOutcome (compression shouldn't break, but we handle it)
compress
  :: ( State state :> es
     , LLM :> es
     , GingerContext compressionContext
     , ToJSON compressionContext
     , FromJSON compressionOutput
     )
  => Compression state history compressionContext compressionOutput event tools
  -> [history]
  -> Eff es (TurnOutcome ())
compress comp history = do
  -- 1. Get current state
  state <- E.get

  -- 2. Build compression context from history and state
  let context = comp.compressionBuildContext history state

  -- 3. Extract template components
  let template = comp.compressionTemplate
      renderFn = render template
      schema = template.templateOutputSchema.schemaJSON
      tools = toolListToJSON template.templateTools

  -- 4. Call LLM to compress
  outcome <- E.runTurn renderFn schema tools context

  -- 5. Apply compression output to state if completed
  case outcome of
    TurnCompleted result -> do
      E.modify (comp.compressionApply result.trOutput)
      return (TurnCompleted ())
    TurnBroken reason ->
      return (TurnBroken reason)

-- | Build context from state (helper)
buildContext :: (state -> context) -> state -> context
buildContext f s = f s
