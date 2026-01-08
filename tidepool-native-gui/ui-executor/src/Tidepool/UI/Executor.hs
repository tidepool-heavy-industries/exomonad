-- | UI effect executor - bridges UI effects to wire types.
--
-- Interprets UI effects by:
-- 1. Accumulating UI state (messages, thinking indicator)
-- 2. Emitting UIState via callback on Request* effects
-- 3. Blocking until UserAction arrives and resuming
--
-- = Usage
--
-- @
-- import Tidepool.UI.Executor (runUI, UICallback)
-- import Tidepool.Effects.UI (UI, showText, requestTextInput)
-- import Tidepool.Wire.Types (UIState, UserAction(..))
--
-- -- Your callback sends UIState over WebSocket, receives UserAction
-- callback :: UICallback
-- callback uiState = do
--   sendOverWebSocket (encode uiState)
--   action <- receiveFromWebSocket
--   pure (decode action)
--
-- main = do
--   ctx <- newUIContext "entry"
--   runUI ctx callback $ do
--     showText "Hello!"
--     name <- requestTextInput "What's your name?"
--     showText ("Nice to meet you, " <> name)
-- @
module Tidepool.UI.Executor
  ( -- * Executor
    runUI

    -- * Callback type
  , UICallback

    -- * Context management
  , UIContext(..)
  , newUIContext
  , setGraphNode
  ) where

import Control.Monad (void, when)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Tidepool.Effects.UI (UI(..), ChoiceMeta(..), ChoiceAvailability(..))
import Tidepool.Wire.Types


-- ════════════════════════════════════════════════════════════════════════════
-- CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Mutable context tracking accumulated UI state.
--
-- The context is modified by ShowText and SetThinking effects.
-- Request* effects read the context to build UIState.
data UIContext = UIContext
  { ucMessages  :: IORef (Seq ChatMessage)  -- ^ Conversation history
  , ucGraphNode :: IORef Text               -- ^ Current graph node name
  , ucThinking  :: IORef Bool               -- ^ Loading indicator
  }

-- | Create a new UI context.
newUIContext :: Text -> IO UIContext
newUIContext initialNode = do
  messages  <- newIORef Seq.empty
  graphNode <- newIORef initialNode
  thinking  <- newIORef False
  pure UIContext
    { ucMessages  = messages
    , ucGraphNode = graphNode
    , ucThinking  = thinking
    }

-- | Update the current graph node.
--
-- Call this when transitioning between graph nodes to keep observability
-- information current.
setGraphNode :: UIContext -> Text -> IO ()
setGraphNode ctx node = writeIORef (ucGraphNode ctx) node

-- | Read current context to build UIState.
readContext :: UIContext -> IO (Seq ChatMessage, Text, Bool)
readContext ctx = do
  msgs <- readIORef (ucMessages ctx)
  node <- readIORef (ucGraphNode ctx)
  think <- readIORef (ucThinking ctx)
  pure (msgs, node, think)

-- | Append a message to the context.
appendMessage :: UIContext -> MessageRole -> Text -> IO ()
appendMessage ctx role content = do
  ts <- iso8601Show <$> getCurrentTime
  let msg = ChatMessage
        { cmRole = role
        , cmContent = content
        , cmTimestamp = T.pack ts
        }
  modifyIORef' (ucMessages ctx) (Seq.|> msg)


-- ════════════════════════════════════════════════════════════════════════════
-- CALLBACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Callback for UI state emission and user action reception.
--
-- The callback is invoked when the graph needs user input:
-- 1. Receives the current UIState (with appropriate affordances enabled)
-- 2. Should send this to the frontend
-- 3. Should block waiting for user response
-- 4. Returns the UserAction from the user
--
-- The server typically implements this by:
-- - Encoding UIState as JSON and sending over WebSocket
-- - Waiting for the next WebSocket message
-- - Decoding and returning the UserAction
type UICallback = UIState -> IO UserAction


-- ════════════════════════════════════════════════════════════════════════════
-- UI STATE BUILDERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build UIState with text input enabled.
buildTextInputState
  :: Seq ChatMessage
  -> Text              -- ^ Graph node
  -> Text              -- ^ Placeholder text
  -> UIState
buildTextInputState msgs node placeholder = UIState
  { usMessages = toList msgs
  , usTextInput = Just TextInputConfig { ticPlaceholder = placeholder }
  , usPhotoUpload = Nothing
  , usChoices = Nothing
  , usGraphNode = node
  , usThinking = False
  }

-- | Build UIState with photo upload enabled.
buildPhotoUploadState
  :: Seq ChatMessage
  -> Text              -- ^ Graph node
  -> Text              -- ^ Prompt text
  -> UIState
buildPhotoUploadState msgs node prompt = UIState
  { usMessages = toList msgs
  , usTextInput = Nothing
  , usPhotoUpload = Just PhotoUploadConfig { pucPrompt = prompt }
  , usChoices = Nothing
  , usGraphNode = node
  , usThinking = False
  }

-- | Build UIState with choices enabled.
buildChoiceState
  :: Seq ChatMessage
  -> Text                            -- ^ Graph node
  -> Text                            -- ^ Prompt
  -> NonEmpty (Text, ChoiceMeta, a)  -- ^ (label, meta, value)
  -> Bool                            -- ^ Multi-select
  -> UIState
buildChoiceState msgs node prompt choices multiSelect = UIState
  { usMessages = toList msgs
  , usTextInput = Nothing
  , usPhotoUpload = Nothing
  , usChoices = Just ChoiceConfig
      { ccPrompt = prompt
      , ccOptions = zipWith mkOption [0..] (NE.toList choices)
      , ccMultiSelect = multiSelect
      }
  , usGraphNode = node
  , usThinking = False
  }
  where
    mkOption idx (label, ChoiceMeta desc costs availability, _) = ChoiceOption
      { coIndex = idx
      , coLabel = label
      , coDescription = desc
      , coCosts = costs
      , coDisabled = availabilityToMaybe availability
      }
    availabilityToMaybe Available = Nothing
    availabilityToMaybe (DisabledBecause reason) = Just reason

-- | Build UIState for simple choices (no metadata).
buildSimpleChoiceState
  :: Seq ChatMessage
  -> Text              -- ^ Graph node
  -> Text              -- ^ Prompt
  -> NonEmpty (Text, a)
  -> Bool              -- ^ Multi-select
  -> UIState
buildSimpleChoiceState msgs node prompt choices multiSelect =
  buildChoiceState msgs node prompt choicesWithMeta multiSelect
  where
    choicesWithMeta = NE.map (\(label, val) -> (label, defaultMeta, val)) choices
    defaultMeta = ChoiceMeta Nothing [] Available

-- | Build UIState for choices with descriptions.
buildDescChoiceState
  :: Seq ChatMessage
  -> Text                          -- ^ Graph node
  -> Text                          -- ^ Prompt
  -> NonEmpty (Text, Text, a)      -- ^ (label, description, value)
  -> UIState
buildDescChoiceState msgs node prompt choices =
  buildChoiceState msgs node prompt choicesWithMeta False
  where
    choicesWithMeta = NE.map (\(label, desc, val) -> (label, ChoiceMeta (Just desc) [] Available, val)) choices

-- | Build UIState with thinking indicator.
buildThinkingState
  :: Seq ChatMessage
  -> Text              -- ^ Graph node
  -> Bool              -- ^ Thinking flag
  -> UIState
buildThinkingState msgs node thinking = UIState
  { usMessages = toList msgs
  , usTextInput = Nothing
  , usPhotoUpload = Nothing
  , usChoices = Nothing
  , usGraphNode = node
  , usThinking = thinking
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run UI effects using a callback for communication.
--
-- This interpreter:
-- 1. Accumulates messages and thinking state in UIContext
-- 2. On Request* effects, builds UIState and invokes callback
-- 3. Blocks on callback, then resumes with the user's response
--
-- The interpreter is polymorphic over remaining effects, enabling composition
-- with other effect interpreters (Habitica, LLM, Observability, etc.).
--
-- = Example
--
-- @
-- ctx <- newUIContext "entry"
-- result <- runM $ runUI ctx myCallback $ do
--   showText "Welcome!"
--   name <- requestTextInput "What's your name?"
--   pure name
-- @
runUI :: LastMember IO effs => UIContext -> UICallback -> Eff (UI ': effs) a -> Eff effs a
runUI ctx callback = interpret $ \case
  ShowText content -> sendM $ do
    -- Append assistant message to context
    appendMessage ctx Assistant content

  SetThinking thinking -> sendM $ do
    -- Update thinking flag
    writeIORef (ucThinking ctx) thinking
    -- If setting thinking to true, emit state update via callback
    -- (This lets the frontend show spinner immediately)
    when thinking $ do
      (msgs, node, _) <- readContext ctx
      let state = buildThinkingState msgs node True
      -- Fire and forget - we don't wait for response
      void $ callback state

  RequestTextInput prompt -> sendM $ do
    -- Build state with text input enabled
    (msgs, node, _) <- readContext ctx
    let state = buildTextInputState msgs node prompt

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract text from response
    case action of
      TextAction content -> do
        -- Record user message in context
        appendMessage ctx User content
        -- Return the text
        pure content
      _ ->
        -- Wrong action type - return empty string
        pure ""

  RequestPhotoInput prompt -> sendM $ do
    -- Build state with photo upload enabled
    (msgs, node, _) <- readContext ctx
    let state = buildPhotoUploadState msgs node prompt

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract photo data from response
    case action of
      PhotoAction base64Data _mimeType ->
        -- Decode base64 to ByteString
        case B64.decode (TE.encodeUtf8 base64Data) of
          Left _ -> pure ""  -- Invalid base64, return empty
          Right bytes -> pure bytes
      _ ->
        -- Wrong action type
        pure ""

  RequestChoice prompt choices -> sendM $ do
    -- Build state with simple choices
    (msgs, node, _) <- readContext ctx
    -- Add prompt as system message
    appendMessage ctx System prompt
    (msgs', _, _) <- readContext ctx
    let state = buildSimpleChoiceState msgs' node prompt choices False

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract choice from response
    extractChoice choices action

  RequestMultiChoice prompt choices -> sendM $ do
    -- Build state with multi-select choices
    (msgs, node, _) <- readContext ctx
    appendMessage ctx System prompt
    (msgs', _, _) <- readContext ctx
    let state = buildSimpleChoiceState msgs' node prompt choices True

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract multiple choices from response
    case action of
      MultiChoiceAction idxs -> do
        let len = NE.length choices
        pure [snd (NE.toList choices !! i) | i <- idxs, i >= 0, i < len]
      ChoiceAction idx -> do
        -- Single choice sent for multi-select - treat as single selection
        let len = NE.length choices
        if idx >= 0 && idx < len
          then pure [snd (NE.toList choices !! idx)]
          else pure []
      _ -> pure []

  RequestChoiceDesc prompt choices -> sendM $ do
    -- Build state with described choices
    (msgs, node, _) <- readContext ctx
    appendMessage ctx System prompt
    (msgs', _, _) <- readContext ctx
    let state = buildDescChoiceState msgs' node prompt choices

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract choice - map back to third element (value)
    extractChoiceDesc choices action

  RequestChoiceMeta prompt choices -> sendM $ do
    -- Build state with full metadata choices
    (msgs, node, _) <- readContext ctx
    appendMessage ctx System prompt
    (msgs', _, _) <- readContext ctx
    let state = buildChoiceState msgs' node prompt choices False

    -- Invoke callback (blocking)
    action <- callback state

    -- Extract choice with disabled validation
    extractChoiceMeta choices action


-- | Extract a single choice from UserAction for simple choices.
extractChoice :: NonEmpty (Text, a) -> UserAction -> IO a
extractChoice choices action = case action of
  ChoiceAction idx
    | idx >= 0 && idx < NE.length choices ->
        pure $ snd (NE.toList choices !! idx)
    | otherwise ->
        -- Invalid index - return first choice as fallback
        pure $ snd (NE.head choices)
  _ ->
    -- Wrong action type - return first choice as fallback
    pure $ snd (NE.head choices)


-- | Extract a single choice from UserAction for described choices.
extractChoiceDesc :: NonEmpty (Text, Text, a) -> UserAction -> IO a
extractChoiceDesc choices action = case action of
  ChoiceAction idx
    | idx >= 0 && idx < NE.length choices ->
        let (_, _, val) = NE.toList choices !! idx in pure val
    | otherwise ->
        let (_, _, val) = NE.head choices in pure val
  _ ->
    let (_, _, val) = NE.head choices in pure val


-- | Extract a single choice from UserAction for meta choices.
-- Validates that disabled options aren't selected.
-- Returns IO error (via fail) if a disabled option is selected.
extractChoiceMeta :: NonEmpty (Text, ChoiceMeta, a) -> UserAction -> IO a
extractChoiceMeta choices action = case action of
  ChoiceAction idx
    | idx >= 0 && idx < NE.length choices ->
        let (_, ChoiceMeta _ _ availability, val) = NE.toList choices !! idx in
        case availability of
          DisabledBecause reason -> fail $ "Selected disabled option: " <> T.unpack reason
          Available -> pure val
    | otherwise ->
        let (_, _, val) = NE.head choices in pure val
  _ ->
    let (_, _, val) = NE.head choices in pure val


-- | Helper to convert Seq to list.
toList :: Seq a -> [a]
toList = foldr (:) []
