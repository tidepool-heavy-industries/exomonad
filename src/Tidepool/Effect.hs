{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Core effect types for Tidepool game loops
module Tidepool.Effect
  ( -- * Core Effects
    GameEffects
  , State(..)
  , Random(..)
  , LLM(..)
  , Emit(..)
  , RequestInput(..)

    -- * Effect Operations
  , get
  , gets
  , put
  , modify
  , randomInt
  , randomDouble
  , runTurn
  , emit
  , requestChoice
  , requestText

    -- * Result Types
  , TurnResult(..)
  , ToolInvocation(..)

    -- * Running Effects (stubs)
  , runGame
  , runState
  , runRandom
  , runLLM
  , runEmit
  , runRequestInput
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Data.Text (Text)
import Data.Aeson (Value)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | The effect stack for game loops
type GameEffects s event =
  '[ LLM
   , RequestInput
   , State s
   , Emit event
   , Random
   ]

-- ══════════════════════════════════════════════════════════════
-- STATE EFFECT
-- ══════════════════════════════════════════════════════════════

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()

type instance DispatchOf (State s) = 'Dynamic

get :: State s :> es => Eff es s
get = send Get

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put = send . Put

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = get >>= put . f

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState _initial = error "TODO: runState - reinterpret using Effectful.State.Static.Local"

-- ══════════════════════════════════════════════════════════════
-- RANDOM EFFECT
-- ══════════════════════════════════════════════════════════════

data Random :: Effect where
  RandomInt :: Int -> Int -> Random m Int  -- lo hi inclusive
  RandomDouble :: Random m Double

type instance DispatchOf Random = 'Dynamic

randomInt :: Random :> es => Int -> Int -> Eff es Int
randomInt lo hi = send (RandomInt lo hi)

randomDouble :: Random :> es => Eff es Double
randomDouble = send RandomDouble

runRandom :: Eff (Random : es) a -> Eff es a
runRandom = error "TODO: runRandom"

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

-- | The LLM effect runs a complete turn with template and tools
-- Interpreter handles: API calls, tool execution loop, retries, parsing
data LLM :: Effect where
  RunTurnOp
    :: Text                          -- Rendered prompt
    -> Value                         -- Output schema (JSON)
    -> [Value]                       -- Tool definitions (JSON)
    -> LLM m (TurnResult Value)      -- Raw JSON output

type instance DispatchOf LLM = 'Dynamic

-- | Run a turn with a template and context
-- This is what library users call
runTurn
  :: forall context output tools es.
     (LLM :> es, ToJSON context, FromJSON output)
  => (context -> Text)    -- render function
  -> Value                -- output schema
  -> [Value]              -- tool definitions
  -> context              -- context to render
  -> Eff es (TurnResult output)
runTurn render schema tools context = do
  let prompt = render context
  rawResult <- send (RunTurnOp prompt schema tools)
  -- Parse the raw JSON output
  -- In real impl, this would use FromJSON; for now just error
  return $ rawResult { trOutput = error "TODO: parse output" }

-- | Result of running an LLM turn
data TurnResult output = TurnResult
  { trOutput :: output                 -- Parsed structured output
  , trToolsInvoked :: [ToolInvocation] -- What tools were called (logging)
  , trNarrative :: Text                -- Any text blocks from response
  }
  deriving (Show, Eq, Generic, Functor)

-- | Record of a tool invocation for logging/debugging
data ToolInvocation = ToolInvocation
  { tiName :: Text
  , tiInput :: Value
  , tiOutput :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LLMConfig = LLMConfig
  { llmApiKey :: Text
  , llmModel :: Text
  , llmMaxTokens :: Int
  }
  deriving (Show, Eq, Generic)

runLLM :: LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM _config = error "TODO: runLLM"

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event :: Effect where
  EmitEvent :: event -> Emit event m ()

type instance DispatchOf (Emit event) = 'Dynamic

emit :: Emit event :> es => event -> Eff es ()
emit = send . EmitEvent

runEmit :: (event -> IO ()) -> Eff (Emit event : es) a -> Eff es a
runEmit _handler = error "TODO: runEmit - interpret by calling handler in IO"

-- ══════════════════════════════════════════════════════════════
-- REQUEST INPUT EFFECT
-- ══════════════════════════════════════════════════════════════

-- | Effect for tools that need external input from the player
-- Interpreter handles: UI presentation, input collection, validation
data RequestInput :: Effect where
  -- | Present choices to player, get their selection
  RequestChoice
    :: Text              -- Prompt to display
    -> [(Text, a)]       -- (label, value) pairs
    -> RequestInput m a

  -- | Request free-form text input
  RequestText
    :: Text              -- Prompt to display
    -> RequestInput m Text

type instance DispatchOf RequestInput = 'Dynamic

-- | Present a choice to the player and get their selection
requestChoice :: RequestInput :> es => Text -> [(Text, a)] -> Eff es a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Request free-form text input from the player
requestText :: RequestInput :> es => Text -> Eff es Text
requestText = send . RequestText

runRequestInput :: Eff (RequestInput : es) a -> Eff es a
runRequestInput = error "TODO: runRequestInput - interpret by presenting UI and collecting input"

-- ══════════════════════════════════════════════════════════════
-- COMBINED RUNNER
-- ══════════════════════════════════════════════════════════════

runGame 
  :: s
  -> LLMConfig
  -> (event -> IO ())
  -> Eff (GameEffects s event) a 
  -> IO (a, s)
runGame _initialState _llmConfig _eventHandler _computation = 
  error "TODO: runGame - compose runState, runRandom, runLLM, runEmit via runEff"
