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
import Control.Monad.Freer
import Control.Monad.Freer.Internal (Eff(..), decomp, qApp)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.IORef
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Tidepool.Effects.UI (UI(..))
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
  , usButtons = Nothing
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
  , usButtons = Nothing
  , usGraphNode = node
  , usThinking = False
  }

-- | Build UIState with buttons (choice) enabled.
buildChoiceState
  :: Seq ChatMessage
  -> Text              -- ^ Graph node
  -> Text              -- ^ Prompt
  -> [(Text, a)]       -- ^ Choices (label, value)
  -> UIState
buildChoiceState msgs node prompt choices = UIState
  { usMessages = toList (msgs Seq.|> promptMsg)
  , usTextInput = Nothing
  , usPhotoUpload = Nothing
  , usButtons = Just $ zipWith mkButton [0..] choices
  , usGraphNode = node
  , usThinking = False
  }
  where
    promptMsg = ChatMessage
      { cmRole = System
      , cmContent = prompt
      , cmTimestamp = ""  -- Will be set properly by the effect
      }
    mkButton :: Int -> (Text, a) -> ButtonConfig
    mkButton idx (label, _) = ButtonConfig
      { bcId = T.pack (show idx)
      , bcLabel = label
      }

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
  , usButtons = Nothing
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
-- = Example
--
-- @
-- ctx <- newUIContext "entry"
-- result <- runUI ctx myCallback $ do
--   showText "Welcome!"
--   name <- requestTextInput "What's your name?"
--   pure name
-- @
runUI :: UIContext -> UICallback -> Eff '[UI] a -> IO a
runUI ctx callback = loop
  where
    loop :: Eff '[UI] a -> IO a
    loop (Val x) = pure x
    loop (E u q) = case decomp u of
      Right (ShowText content) -> do
        -- Append assistant message to context
        appendMessage ctx Assistant content
        -- Continue (no callback needed, just state update)
        loop (qApp q ())

      Right (SetThinking thinking) -> do
        -- Update thinking flag
        writeIORef (ucThinking ctx) thinking
        -- If setting thinking to true, emit state update via callback
        -- (This lets the frontend show spinner immediately)
        when thinking $ do
          (msgs, node, _) <- readContext ctx
          let state = buildThinkingState msgs node True
          -- Fire and forget - we don't wait for response
          void $ callback state
        -- Continue
        loop (qApp q ())

      Right (RequestTextInput prompt) -> do
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
            -- Continue with the text
            loop (qApp q content)
          _ ->
            -- Wrong action type - this shouldn't happen if frontend is correct
            -- For robustness, return empty string and continue
            loop (qApp q "")

      Right (RequestPhotoInput prompt) -> do
        -- Build state with photo upload enabled
        (msgs, node, _) <- readContext ctx
        let state = buildPhotoUploadState msgs node prompt

        -- Invoke callback (blocking)
        action <- callback state

        -- Extract photo data from response
        case action of
          PhotoAction base64Data _mimeType -> do
            -- Decode base64 to ByteString
            case B64.decode (TE.encodeUtf8 base64Data) of
              Left _ -> loop (qApp q "")  -- Invalid base64, return empty
              Right bytes -> loop (qApp q bytes)
          _ ->
            -- Wrong action type
            loop (qApp q "")

      Right (RequestChoice prompt choices) -> do
        -- Build state with buttons
        (msgs, node, _) <- readContext ctx
        -- Add prompt as system message
        appendMessage ctx System prompt
        (msgs', _, _) <- readContext ctx
        let state = buildChoiceState msgs' node prompt choices

        -- Invoke callback (blocking)
        action <- callback state

        -- Extract choice from response
        case action of
          ButtonAction buttonId -> do
            -- Parse button ID as index
            case reads (T.unpack buttonId) of
              [(idx, "")] | idx >= 0 && idx < length choices ->
                -- Return the value at that index
                loop (qApp q (snd (choices !! idx)))
              _ ->
                -- Invalid index - return first choice as fallback
                case choices of
                  ((_, val):_) -> loop (qApp q val)
                  [] -> error "RequestChoice called with empty choices"
          _ ->
            -- Wrong action type - return first choice as fallback
            case choices of
              ((_, val):_) -> loop (qApp q val)
              [] -> error "RequestChoice called with empty choices"

      Left _ -> error "Impossible: no other effects in stack"

-- | Helper to convert Seq to list.
toList :: Seq a -> [a]
toList = foldr (:) []
