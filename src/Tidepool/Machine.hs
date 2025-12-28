{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
-- | State Machine Agent Infrastructure
--
-- This module provides the core abstraction for building LLM agents
-- as state machines. Each "phase" of the machine:
--
-- * Has its own prompt template
-- * Has its own available tools
-- * Has its own output schema
-- * Can transition to other phases via tools or conditions
--
-- Users declare their machine at the type level, provide phase configs,
-- and the library handles the turn loop.
module Tidepool.Machine
  ( -- * Type Families (user instantiates)
    PhaseTools
  , PhaseOutput
  , Transitions

    -- * Phase Configuration
  , PhaseConfig(..)
  , MachineConfig(..)

    -- * Phase Wrapper
  , SomePhase(..)
  , withSomePhase

    -- * Conditions
  , Condition(..)
  , condition

    -- * Running the Machine
  , runMachineTurn
  , runMachine
  , MachineEffects

    -- * Re-exports
  , Proxy(..)

    -- * Tool Derivation (re-exported from Tidepool.Tool)
  , ReifyToolList(..)
  , toolsFromType
  , dispatcherFromType
  ) where

import Effectful
import Data.Text (Text)
import Data.Aeson (Value, FromJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Control.Monad (forM_)

import Tidepool.Effect
import Tidepool.Tool (ReifyToolList(..), toolsFromType, dispatcherFromType)

-- ══════════════════════════════════════════════════════════════
-- TYPE FAMILIES (users instantiate these)
-- ══════════════════════════════════════════════════════════════

-- | Tools available in each phase
type family PhaseTools (p :: k) :: [Type]

-- | Output type for each phase
type family PhaseOutput (p :: k) :: Type

-- | Valid transitions from each phase
type family Transitions (p :: k) :: [k]

-- ══════════════════════════════════════════════════════════════
-- PHASE CONFIGURATION
-- ══════════════════════════════════════════════════════════════

-- | Configuration for a single phase
data PhaseConfig world ctx output = PhaseConfig
  { pcRender       :: ctx -> Text
  , pcSchema       :: Value
  , pcTools        :: [Value]
  , pcBuildContext :: world -> ctx
  , pcApply        :: output -> world -> world
  }

-- | Full machine configuration
--
-- Note: We use simple functions instead of rank-2 types for compatibility
-- with OverloadedRecordDot. The phase config function takes SomePhase.
data MachineConfig phase world ctx output event = MachineConfig
  { mcPhaseConfig   :: SomePhase phase -> PhaseConfig world ctx output
    -- ^ Get config for any phase (wrapped)
  , mcDispatcher    :: ToolDispatcher event
                        '[ State world, Emit event, RequestInput, Random ]
    -- ^ Tool dispatcher
  , mcGetPhase      :: world -> SomePhase phase
    -- ^ Extract current phase from world
  , mcSetPhase      :: SomePhase phase -> world -> world
    -- ^ Set phase in world
  , mcPhaseEq       :: SomePhase phase -> SomePhase phase -> Bool
    -- ^ Check if two phases are the same
  , mcLLMConfig     :: LLMConfig
    -- ^ LLM configuration
  , mcConditions    :: [Condition world phase]
    -- ^ Conditions checked after each turn
  , mcOnEvent       :: event -> IO ()
    -- ^ Event handler
  , mcInputHandler  :: InputHandler
    -- ^ Handler for player input requests
  , mcLogLevel      :: LogLevel
    -- ^ Minimum log level
  }

-- ══════════════════════════════════════════════════════════════
-- SOME PHASE (existential wrapper)
-- ══════════════════════════════════════════════════════════════

-- | Existentially wrapped phase for storage
data SomePhase (phase :: k -> Type) where
  SomePhase :: phase p -> SomePhase phase

-- | Unwrap to work with contained phase
withSomePhase :: SomePhase phase -> (forall p. phase p -> r) -> r
withSomePhase (SomePhase p) f = f p

-- ══════════════════════════════════════════════════════════════
-- CONDITIONS
-- ══════════════════════════════════════════════════════════════

-- | A condition that can trigger phase transitions
data Condition world phase = Condition
  { condName  :: Text
  , condCheck :: world -> Maybe (SomePhase phase)
  }

-- | Create a condition
condition :: Text -> (world -> Maybe (SomePhase phase)) -> Condition world phase
condition = Condition

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE MACHINE
-- ══════════════════════════════════════════════════════════════

-- | The effect stack for machine operations
type MachineEffects world event =
  '[ LLM
   , RequestInput
   , Log
   , ChatHistory
   , State world
   , Emit event
   , Random
   , IOE
   ]

-- | Run a single turn of the machine
runMachineTurn
  :: forall phase world ctx output event.
     ( FromJSON output )
  => MachineConfig phase world ctx output event
  -> Text  -- ^ User input
  -> Eff (MachineEffects world event) (TurnParseResult output)
runMachineTurn config userInput = loop
  where
    loop :: Eff (MachineEffects world event) (TurnParseResult output)
    loop = do
      -- Get current phase
      world <- get @world
      let currentPhase = config.mcGetPhase world
          cfg = config.mcPhaseConfig currentPhase

      -- Build context and render prompt
      let ctx = cfg.pcBuildContext world
          systemPrompt = cfg.pcRender ctx

      logDebug "[Machine] Running turn"

      -- Call LLM
      outcome <- runTurn @output
        systemPrompt
        userInput
        cfg.pcSchema
        cfg.pcTools

      case outcome of
        TurnBroken reason -> do
          logInfo $ "[Machine] Tool broke turn: " <> reason
          loop

        TurnCompleted parseResult -> case parseResult of
          TurnParseFailed{} -> do
            logWarn "[Machine] Parse failed"
            return parseResult

          TurnParsed result -> do
            phaseBefore <- gets @world config.mcGetPhase

            modify @world (cfg.pcApply result.trOutput)

            forM_ config.mcConditions $ \cond -> do
              w <- get @world
              case cond.condCheck w of
                Nothing -> return ()
                Just newPhase -> do
                  logInfo $ "[Machine] Condition triggered: " <> cond.condName
                  modify @world (config.mcSetPhase newPhase)

            phaseAfter <- gets @world config.mcGetPhase

            if config.mcPhaseEq phaseBefore phaseAfter
              then return parseResult
              else do
                logInfo "[Machine] Phase changed, restarting"
                loop

-- | Run the machine in a loop with IO
runMachine
  :: forall phase world ctx output event.
     ( FromJSON output )
  => MachineConfig phase world ctx output event
  -> world
  -> (Text -> IO Text)
  -> IO world
runMachine config initialWorld getInput = do
  ((), finalWorld) <- runEff
    . runRandom
    . runEmit config.mcOnEvent
    . runState initialWorld
    . runChatHistory
    . runLog config.mcLogLevel
    . runRequestInput config.mcInputHandler
    . runLLM config.mcLLMConfig
    $ gameLoop

  return finalWorld
  where
    gameLoop :: Eff (MachineEffects world event) ()
    gameLoop = do
      userInput <- liftIO $ getInput "> "
      _ <- runMachineTurn config userInput
      gameLoop
