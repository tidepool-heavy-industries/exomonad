-- | Observability effect for structured logging and tracing.
--
-- Effect type only - executors live in tidepool-observability-executor.
module Tidepool.Effects.Observability
  ( -- * Effect
    Observability(..)
  , publishEvent
  , withSpan

    -- * Event Types
  , TidepoolEvent(..)
  ) where

import Data.Text (Text)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send)


-- ════════════════════════════════════════════════════════════════════════════
-- EVENT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Structured events for observability.
--
-- These events are published to Loki for queryability and dashboard visualization.
data TidepoolEvent
  = GraphTransition
      { getFrom :: Text
      , getTo :: Text
      , getTrigger :: Text
      }
  | LLMCallEvent
      { llmModel :: Text
      , llmPromptTokens :: Int
      , llmCompletionTokens :: Int
      , llmLatencyMs :: Int
      }
  | UserActionEvent
      { uaActionType :: Text
      , uaNodeContext :: Text
      }
  | EffectExecutionEvent
      { eeEffectType :: Text
      , eeSuccess :: Bool
      , eeLatencyMs :: Int
      }
  | ErrorEvent
      { errMessage :: Text
      , errContext :: Value
      }
  deriving (Show, Eq, Generic)

instance ToJSON TidepoolEvent where
  toJSON (GraphTransition from_ to_ trigger_) = object
    [ "type" .= ("graph_transition" :: Text)
    , "from" .= from_
    , "to" .= to_
    , "trigger" .= trigger_
    ]
  toJSON (LLMCallEvent model_ prompt_ completion_ latency_) = object
    [ "type" .= ("llm_call" :: Text)
    , "model" .= model_
    , "prompt_tokens" .= prompt_
    , "completion_tokens" .= completion_
    , "latency_ms" .= latency_
    ]
  toJSON (UserActionEvent action_ context_) = object
    [ "type" .= ("user_action" :: Text)
    , "action_type" .= action_
    , "node_context" .= context_
    ]
  toJSON (EffectExecutionEvent effect_ success_ latency_) = object
    [ "type" .= ("effect_execution" :: Text)
    , "effect_type" .= effect_
    , "success" .= success_
    , "latency_ms" .= latency_
    ]
  toJSON (ErrorEvent msg_ ctx_) = object
    [ "type" .= ("error" :: Text)
    , "message" .= msg_
    , "context" .= ctx_
    ]

instance FromJSON TidepoolEvent where
  parseJSON = withObject "TidepoolEvent" $ \v -> do
    eventType <- v .: "type" :: Aeson.Parser Text
    case eventType of
      "graph_transition" ->
        GraphTransition <$> v .: "from" <*> v .: "to" <*> v .: "trigger"
      "llm_call" ->
        LLMCallEvent <$> v .: "model" <*> v .: "prompt_tokens"
                     <*> v .: "completion_tokens" <*> v .: "latency_ms"
      "user_action" ->
        UserActionEvent <$> v .: "action_type" <*> v .: "node_context"
      "effect_execution" ->
        EffectExecutionEvent <$> v .: "effect_type" <*> v .: "success" <*> v .: "latency_ms"
      "error" ->
        ErrorEvent <$> v .: "message" <*> v .: "context"
      _ -> fail $ "Unknown event type: " ++ show eventType


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Observability effect for publishing events and tracing spans.
data Observability r where
  PublishEvent :: TidepoolEvent -> Observability ()
  WithSpan :: Text -> Eff '[Observability] a -> Observability a


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Publish an observability event.
publishEvent :: Member Observability effs => TidepoolEvent -> Eff effs ()
publishEvent = send . PublishEvent

-- | Execute an action within a named span for tracing.
withSpan :: Member Observability effs => Text -> Eff '[Observability] a -> Eff effs a
withSpan name action = send (WithSpan name action)
