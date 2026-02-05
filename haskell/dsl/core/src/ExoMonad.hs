{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ExoMonad: Opinionated LLM Agent Framework
--
-- An agent is (State, Event, Run). That's the opinion.
-- The agent owns its lifecycle. The runner just interprets effects.
--
-- = IO-Blindness
--
-- Agents are IO-blind: they cannot use IOE directly. All IO happens in the
-- runner. This enables eventual WASM compilation.
--
-- = Extensible Effects
--
-- Base effects are fixed for all agents. Agents can add domain-specific
-- effects (like Habitica, Obsidian) via the @extra@ type parameter.
-- The runner provides interpreters for extra effects.
--
-- = Runners
--
-- For platform-specific runners that interpret effects with real IO (HTTP, SQLite, GUI),
-- see @ExoMonad.Effect.Runners@ in exomonad-platform.
module ExoMonad
  ( -- * Agent Definition
    Agent (..),
    SimpleAgent,
    AgentM,
    BaseEffects,
    type (++),

    -- * Tool Dispatcher
    AgentDispatcher (..),
    noDispatcher,

    -- * Configuration (for runners in exomonad-platform)
    AgentConfig (..),

    -- * Re-exports
    module ExoMonad.Effect,
  )
where

import Data.Aeson (Value, toJSON)
import Data.Kind (Type)
import ExoMonad.Effect
import Polysemy (Member, Sem)
import Prelude hiding (State)

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
  { -- | Human-readable name for logging
    agentName :: Text,
    -- | Initial state (before agentRun starts)
    agentInit :: s,
    -- | The agent's entire lifecycle. Uses RequestInput when it needs input,
    -- Emit for output, returns () when session is complete.
    agentRun :: AgentM s evt extra (),
    -- | Tool dispatcher for LLM tool calls (use 'noDispatcher' if no tools)
    agentDispatcher :: AgentDispatcher s evt
  }

-- | Effect type alias following Polysemy's design.
--
-- Polysemy represents effects with the kind @(Type -> Type) -> Type -> Type@:
--
-- @
-- data MyEffect (m :: Type -> Type) (a :: Type) where
--   -- constructors use @m@ for subcomputations and return @a@
-- @
--
-- Earlier versions of some effect systems (and code in this project) used
-- effects of kind @Type -> Type@ instead. Moving to Polysemy's kind is a
-- breaking change for any custom effects: all user-defined effects must now
-- take both the monad parameter @m@ and the result type @a@ explicitly, as
-- in the example above.
--
-- For guidance on updating existing custom effects to this representation,
-- see the Polysemy effect definition documentation.
type Effect = (Type -> Type) -> Type -> Type

-- | Convenience alias for agents with no extra effects
type SimpleAgent s evt = Agent s evt '[]

-- | The agent monad. Extra effects prepended to base effects.
type AgentM s evt extra = Eff (extra ++ BaseEffects s evt)

-- | Base effects for all agents. IO-blind by design.
--
-- Agents cannot use IO directly - it's not in the stack. The runner injects IO
-- at the interpretation boundary. This enables WASM compilation where
-- IO is provided by the host environment.
--
-- Interpreters like @runRandom@ require @Member (Embed IO) effs@, but that constraint
-- applies to the /remaining/ stack after interpretation, not the agent's
-- visible stack.
type BaseEffects s evt =
  '[ LLM,
     State s,
     Emit evt,
     RequestInput,
     Log,
     ChatHistory,
     Random,
     Time
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
  { runDispatcher ::
      forall effs.
      ( Member (State s) effs,
        Member (Emit evt) effs,
        Member RequestInput effs,
        Member Random effs,
        Member Log effs
      ) =>
      Text -> -- Tool name
      Value -> -- Tool input (JSON)
      Eff effs (Either Text ToolResult) -- Simplified ToolResult (no targets)
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
  { -- | Handle each event as it's emitted (display, log, etc.)
    acOnEvent :: evt -> IO (),
    -- | Periodic persistence hook
    acOnSave :: s -> IO (),
    -- | Get user input for next turn
    acGetInput :: IO Text,
    -- | LLM API configuration
    acLLM :: LLMConfig,
    -- | Minimum log level to display
    acLogLevel :: LogLevel,
    -- | Commands that end the session (e.g., ["quit", "exit"])
    acQuitCommands :: [Text]
  }

-- Note: Platform-specific runners (exomonad, exomonadWith) are in
-- ExoMonad.Effect.Runners in the exomonad-platform package.
-- They interpret BaseEffects using real IO (HTTP, SQLite, etc.).
