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
  , LLMConfig(..)
  , InputHandler(..)

    -- * Running Effects
  , runGame
  , runState
  , runRandom
  , runLLM
  , runEmit
  , runRequestInput
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import System.Random (randomRIO)
import Data.Text (Text)
import Data.Aeson (Value, FromJSON, ToJSON, fromJSON, toJSON, Result(..))
import GHC.Generics (Generic)
import qualified Tidepool.Anthropic.Client as Client
import qualified Tidepool.Anthropic.Http as Http

-- | The effect stack for game loops
-- IOE is at the base for IO operations (random, emit, input)
type GameEffects s event =
  '[ LLM
   , RequestInput
   , State s
   , Emit event
   , Random
   , IOE
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
runState initial = reinterpret (EState.runState initial) $ \_ -> \case
  Get   -> EState.get
  Put s -> EState.put s

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

runRandom :: IOE :> es => Eff (Random : es) a -> Eff es a
runRandom = interpret $ \_ -> \case
  RandomInt lo hi -> liftIO $ randomRIO (lo, hi)
  RandomDouble    -> liftIO $ randomRIO (0.0, 1.0)

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
  -- Parse the raw JSON output using FromJSON
  case fromJSON rawResult.trOutput of
    Success parsed -> return rawResult { trOutput = parsed }
    Error err -> error $ "Failed to parse LLM output: " <> err

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

-- | Run the LLM effect by calling the Anthropic API
-- Note: Tool execution currently uses a stub that logs tool calls
-- For full tool support, use runLLMWithTools
runLLM :: IOE :> es => LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM config = interpret $ \_ -> \case
  RunTurnOp prompt schema tools -> liftIO $ do
    let clientConfig = Client.ClientConfig
          { Client.apiKey = config.llmApiKey
          , Client.defaultModel = config.llmModel
          , Client.defaultMaxTokens = config.llmMaxTokens
          }
        -- Pass schema for structured output
        outputSchema = if schema == toJSON () then Nothing else Just schema
        turnReq = Client.TurnRequest
          { Client.prompt = prompt
          , Client.systemPrompt = Nothing  -- Could be added to LLMConfig
          , Client.outputSchema = outputSchema
          , Client.tools = tools
          , Client.toolExecutor = stubToolExecutor
          }

    result <- Client.runTurnRequest clientConfig turnReq
    case result of
      Left err -> error $ "LLM API error: " <> show err
      Right resp -> pure TurnResult
        { trOutput = resp.output
        , trToolsInvoked = map convertInvocation resp.toolsInvoked
        , trNarrative = resp.narrative
        }
  where
    -- Stub tool executor - just returns success with empty result
    -- Real implementation would need to dispatch to actual tool handlers
    stubToolExecutor :: Text -> Value -> IO (Either Text Value)
    stubToolExecutor _name _input = pure $ Right (toJSON ())

    convertInvocation :: Client.ToolInvocation -> ToolInvocation
    convertInvocation inv = ToolInvocation
      { tiName = inv.invocationName
      , tiInput = inv.invocationInput
      , tiOutput = inv.invocationOutput
      }

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event :: Effect where
  EmitEvent :: event -> Emit event m ()

type instance DispatchOf (Emit event) = 'Dynamic

emit :: Emit event :> es => event -> Eff es ()
emit = send . EmitEvent

runEmit :: IOE :> es => (event -> IO ()) -> Eff (Emit event : es) a -> Eff es a
runEmit handler = interpret $ \_ -> \case
  EmitEvent e -> liftIO $ handler e

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

-- | Handler for input requests (terminal, UI, etc.)
data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  }

runRequestInput :: IOE :> es => InputHandler -> Eff (RequestInput : es) a -> Eff es a
runRequestInput (InputHandler choiceHandler textHandler) = interpret $ \_ -> \case
  RequestChoice prompt choices -> liftIO $ choiceHandler prompt choices
  RequestText prompt           -> liftIO $ textHandler prompt

-- ══════════════════════════════════════════════════════════════
-- COMBINED RUNNER
-- ══════════════════════════════════════════════════════════════

-- | Run the full game effect stack
runGame
  :: s
  -> LLMConfig
  -> (event -> IO ())
  -> InputHandler
  -> Eff (GameEffects s event) a
  -> IO (a, s)
runGame initialState llmConfig eventHandler inputHandler computation =
  runEff
    . runRandom
    . runEmit eventHandler
    . runState initialState
    . runRequestInput inputHandler
    . runLLM llmConfig
    $ computation
