{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Tidepool: Opinionated LLM Agent Framework
--
-- An agent is (State, Event, Run). That's the opinion.
-- The agent owns its lifecycle. The runner just interprets effects.
--
-- = IO-Blindness
--
-- Agents are IO-blind: they cannot use IOE directly. All IO happens in the
-- runner (tidepool/tidepoolWith). This enables eventual WASM compilation.
--
-- = Extensible Effects
--
-- Base effects are fixed for all agents. Agents can add domain-specific
-- effects (like Habitica, Obsidian) via the @extra@ type parameter.
-- The runner provides interpreters for extra effects.
--
module Tidepool
  ( -- * Agent Definition
    Agent(..)
  , SimpleAgent
  , AgentM
  , BaseEffects
  , type (++)

    -- * Tool Dispatcher
  , AgentDispatcher(..)
  , noDispatcher

    -- * Running Agents
  , tidepool
  , tidepoolWith
  , AgentConfig(..)

    -- * Re-exports
  , module Tidepool.Effect
  ) where

import Effectful
import Data.Text (Text)
import Data.Aeson (Value, toJSON)
import Tidepool.Effect

-- ══════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ══════════════════════════════════════════════════════════════════════

-- | Type-level list append
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- ══════════════════════════════════════════════════════════════════════
-- THE OPINION: An agent is (State, Event, Run)
-- ══════════════════════════════════════════════════════════════════════

-- | An agent. Parameterized by state, event, and extra effect types.
--
-- The @extra@ parameter allows agents to use domain-specific effects
-- beyond the base set. Use @'[]@ for simple agents.
--
-- = Design: Agent as Process
--
-- The agent owns its entire lifecycle via 'agentRun'. It uses 'requestText'
-- when it needs input, 'emit' when it has output, and returns when done.
-- The runner just interprets effects - it doesn't impose loop structure.
data Agent s evt (extra :: [Effect]) = Agent
  { agentName       :: Text
    -- ^ Human-readable name for logging
  , agentInit       :: s
    -- ^ Initial state (before agentRun starts)
  , agentRun        :: AgentM s evt extra ()
    -- ^ The agent's entire lifecycle. Uses RequestInput when it needs input,
    -- Emit for output, returns () when session is complete.
  , agentDispatcher :: AgentDispatcher s evt
    -- ^ Tool dispatcher for LLM tool calls (use 'noDispatcher' if no tools)
  }

-- | Convenience alias for agents with no extra effects
type SimpleAgent s evt = Agent s evt '[]

-- | The agent monad. Extra effects prepended to base effects.
type AgentM s evt extra = Eff (extra ++ BaseEffects s evt)

-- | Base effects for all agents. IO-blind by design.
--
-- Agents cannot use IOE - it's not in the stack. The runner injects IOE
-- at the interpretation boundary. This enables WASM compilation where
-- IO is provided by the host environment.
--
-- Interpreters like @runRandom@ require @IOE :> es@, but that constraint
-- applies to the /remaining/ stack after interpretation, not the agent's
-- visible stack.
type BaseEffects s evt =
  '[ LLM
   , State s
   , Emit evt
   , RequestInput
   , Log
   , ChatHistory
   , Random
   ]

-- ══════════════════════════════════════════════════════════════════════
-- TOOL DISPATCHER
-- ══════════════════════════════════════════════════════════════════════

-- | Dispatcher for tool execution.
--
-- Tools run in the agent's effect context (State, Emit, RequestInput, Random)
-- but NOT IO. This keeps agents IO-blind.
--
-- Use 'noDispatcher' for agents without tools.
newtype AgentDispatcher s evt = AgentDispatcher
  { runDispatcher
      :: forall es.
         ( State s :> es
         , Emit evt :> es
         , RequestInput :> es
         , Random :> es
         , Log :> es
         )
      => Text   -- Tool name
      -> Value  -- Tool input (JSON)
      -> Eff es (Either Text ToolResult)
  }

-- | Default dispatcher for agents without tools
noDispatcher :: AgentDispatcher s evt
noDispatcher = AgentDispatcher $ \_ _ ->
  pure (Right (ToolSuccess (toJSON ())))

-- ══════════════════════════════════════════════════════════════════════
-- CONFIGURATION (runtime, not behavior)
-- ══════════════════════════════════════════════════════════════════════

-- | Configuration for running an agent.
--
-- Events stream via callback rather than accumulating in memory.
-- This supports long sessions (DM campaigns) without memory growth.
data AgentConfig s evt = AgentConfig
  { acOnEvent     :: evt -> IO ()
    -- ^ Handle each event as it's emitted (display, log, etc.)
  , acOnSave      :: s -> IO ()
    -- ^ Periodic persistence hook
  , acGetInput    :: IO Text
    -- ^ Get user input for next turn
  , acLLM         :: LLMConfig
    -- ^ LLM API configuration
  , acLogLevel    :: LogLevel
    -- ^ Minimum log level to display
  , acQuitCommands :: [Text]
    -- ^ Commands that end the session (e.g., ["quit", "exit"])
  }

-- ══════════════════════════════════════════════════════════════════════
-- RUNNER
-- ══════════════════════════════════════════════════════════════════════

-- | Run a simple agent (no extra effects). Returns final state.
--
-- Events stream via 'acOnEvent' callback, not collected.
--
-- @
-- main = do
--   finalState <- tidepool config myAgent
--   putStrLn $ "Final score: " <> show finalState.score
-- @
tidepool :: AgentConfig s evt -> SimpleAgent s evt -> IO s
tidepool config agent = tidepoolWith config id agent

-- | Run an agent with extra effects.
--
-- You must provide an interpreter for the extra effects. The interpreter
-- runs in an IO context and can use IOE.
--
-- @
-- runDelta = tidepoolWith config runExternalEffects delta
--   where
--     runExternalEffects = runHabitica . runObsidian . runGitHub . runCalendar
-- @
--
-- TODO: Full implementation pending effect stack composition work.
-- The type-level list append (++) requires careful handling with effectful.
tidepoolWith
  :: forall s evt extra.
     AgentConfig s evt
  -> (forall es a. IOE :> es => Eff (extra ++ es) a -> Eff es a)
  -> Agent s evt extra
  -> IO s
tidepoolWith _config _extraInterp _agent =
  error "TODO: tidepoolWith - implement effect stack composition"
