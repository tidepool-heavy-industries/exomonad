-- | Generic GUI widgets for Tidepool
--
-- These widgets can be used by any Tidepool-based game. Domain-specific
-- widgets (like dice with tier colors) are in their respective modules.
--
-- = Design Philosophy
--
-- These widgets are intentionally simple and generic:
--
-- * 'narrativePane' just renders Text entries - domain modules can
--   provide richer narrative rendering (e.g., NPC speech bubbles)
-- * 'textInput' and 'choiceCards' handle the MVar communication
-- * Update functions are exposed for external refresh triggers
--
-- = Integration Pattern
--
-- For domain-specific rendering, wrap these widgets or create parallel
-- implementations in your domain's GUI module. For example:
--
-- @
-- -- In DM.GUI.Widgets.Narrative
-- dmNarrativePane :: GUIBridge WorldState -> UI Element
-- dmNarrativePane bridge = do
--   -- Read scene beats instead of plain text
--   -- Render with speech bubbles, player action styling, etc.
-- @
--
module Tidepool.GUI.Widgets
  ( -- * Input widgets
    textInput
  , choiceCards
    -- * Display widgets
  , narrativePane
  , debugPanel
  , stateInspector
  , StateInspectorHandle(..)
    -- * Update functions
  , updateNarrative
  , updateDebugPanel
  , refreshStateInspector
    -- * Loading state
  , loadingOverlay
    -- * Layout helpers
  , withClass
  , addClass
  , focusElement
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void, when)
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Format (formatTime, defaultTimeLocale)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core

-- | Create a text input widget
--
-- When the user submits (Enter key or button click), the response
-- is put into the bridge's MVar. The input is auto-focused when created.
textInput
  :: GUIBridge state
  -> Text           -- ^ Prompt/placeholder
  -> UI Element
textInput bridge prompt = do
  container <- UI.div #. "text-input-container"

  -- Prompt label above the input
  promptEl <- UI.div #. "input-prompt" # set text (T.unpack prompt)

  -- Input row: input field + submit button
  inputRow <- UI.div #. "text-input-row"

  inputEl <- UI.input #. "text-input"
    # set (attr "placeholder") "Type your response..."
    # set (attr "autocomplete") "off"

  submitBtn <- UI.button #. "submit-btn" # set text "Submit"

  let submit = do
        val <- get value inputEl
        liftIO $ TIO.hPutStrLn stderr $ "[textInput] Submit triggered, value: " <> T.pack (show val)
        when (not (null val)) $ do
          liftIO $ TIO.hPutStrLn stderr $ "[textInput] Submitting: " <> T.pack val
          liftIO $ safeSubmitResponse bridge (TextResponse (T.pack val))
        void $ element inputEl # set value ""

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element inputRow #+ [element inputEl, element submitBtn]
  void $ element container #+ [element promptEl, element inputRow]

  -- Auto-focus the input
  focusElement inputEl

  pure container

-- | Create choice cards widget
--
-- Renders a set of clickable cards. When clicked, the response
-- is put into the bridge's MVar.
--
-- Features:
-- - Click to select
-- - Keyboard navigation: Tab between cards, Enter/Space to select
-- - Number keys (1-9) for quick selection
choiceCards
  :: GUIBridge state
  -> Text                -- ^ Prompt
  -> [(Text, Int)]       -- ^ (label, index) pairs
  -> UI Element
choiceCards bridge prompt options = do
  wrapper <- UI.div #. "choice-wrapper"

  promptEl <- UI.div #. "choice-prompt" # set text (T.unpack prompt)

  -- Cards container with keyboard handling
  cardsContainer <- UI.div #. "choice-container"
    # set (attr "role") "listbox"
    # set (attr "aria-label") "Choose an option"

  -- Create cards with indices for keyboard shortcuts
  cards <- mapM (mkCard bridge) (zip [1..] options)

  void $ element cardsContainer #+ map element cards

  -- Set up keyboard shortcuts on the container
  on UI.keydown cardsContainer $ \code -> do
    -- Number keys 1-9 for quick selection
    when (code >= 49 && code <= 57) $ do
      let num = code - 48  -- Convert keycode to 1-9
      when (num <= length options) $ do
        let (_, idx) = options !! (num - 1)
        liftIO $ safeSubmitResponse bridge (ChoiceResponse idx)

  void $ element wrapper #+ [element promptEl, element cardsContainer]
  pure wrapper

mkCard :: GUIBridge state -> (Int, (Text, Int)) -> UI Element
mkCard bridge (num, (label, idx)) = do
  card <- UI.div #. "choice-card"
    # set (attr "tabindex") "0"  -- Make focusable
    # set (attr "role") "option"
    # set (attr "aria-label") (T.unpack $ "Option " <> T.pack (show num) <> ": " <> label)

  -- Show number hint for keyboard shortcut
  numHint <- UI.span #. "choice-num" # set text (show num <> ". ")
  labelEl <- UI.span # set text (T.unpack label)

  -- Click handler
  on UI.click card $ const $ liftIO $
    safeSubmitResponse bridge (ChoiceResponse idx)

  -- Keyboard handler (Enter or Space to select)
  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $  -- Enter or Space
      safeSubmitResponse bridge (ChoiceResponse idx)

  void $ element card #+ [element numHint, element labelEl]
  pure card

-- | Create a narrative display pane
--
-- Shows the narrative log from the bridge, auto-scrolling to bottom.
narrativePane :: GUIBridge state -> UI Element
narrativePane bridge = do
  container <- UI.div #. "narrative-pane"

  -- Initial render
  updateNarrative container bridge

  pure container

-- | Update the narrative pane with current log entries
updateNarrative :: Element -> GUIBridge state -> UI ()
updateNarrative container bridge = do
  entries <- liftIO $ atomically $ readTVar bridge.gbNarrativeLog
  let entryEls = map mkNarrativeEntry (toList entries)
  void $ element container # set children []
  void $ element container #+ entryEls
  -- Auto-scroll to bottom
  runFunction $ ffi "$(%1).scrollTop($(%1)[0].scrollHeight)" container

-- | Create a narrative entry from a ChatMessage
--
-- For the generic widget, we just extract text content.
-- Domain-specific widgets can pattern-match for richer rendering.
mkNarrativeEntry :: ChatMessage -> UI Element
mkNarrativeEntry msg =
  UI.div #. "narrative-entry" # set text (T.unpack $ chatMessageText msg)

-- | Extract text content from a ChatMessage (for simple rendering)
chatMessageText :: ChatMessage -> Text
chatMessageText (SystemMessage txt) = txt
chatMessageText (UserMessage txt) = "> " <> txt
chatMessageText (PhotoMessage _ _) = "[Photo]"
chatMessageText (ChoicesMessage prompt _) = prompt
chatMessageText (SelectedMessage txt) = "✓ " <> txt
chatMessageText (ErrorMessage txt) = "⚠ " <> txt

-- | Create a debug panel
--
-- Shows debug log entries from the bridge.
debugPanel :: GUIBridge state -> UI Element
debugPanel bridge = do
  panel <- UI.div #. "debug-panel"

  titleEl <- UI.div #. "debug-section-title" # set text "DEBUG LOG"
  contentEl <- UI.div #. "debug-content"

  -- Initial render
  updateDebugPanel contentEl bridge

  void $ element panel #+ [element titleEl, element contentEl]
  pure panel

-- | Update the debug panel with current log entries
updateDebugPanel :: Element -> GUIBridge state -> UI ()
updateDebugPanel container bridge = do
  entries <- liftIO $ atomically $ readTVar bridge.gbDebugLog
  let entryEls = map mkDebugEntry (toList entries)
  void $ element container # set children []
  void $ element container #+ entryEls

mkDebugEntry :: DebugEntry -> UI Element
mkDebugEntry entry = do
  el <- UI.div #. "debug-entry"

  let levelClass = case entry.deLevel of
        Debug -> "debug"
        Info  -> "info"
        Warn  -> "warn"
        Error -> "error"

  -- Timestamp
  let timeStr = formatTime defaultTimeLocale "%H:%M:%S" entry.deTimestamp
  timeEl <- UI.span #. "debug-time" # set text timeStr

  -- Level badge
  levelEl <- UI.span #. ("debug-level " <> levelClass)
    # set text (show entry.deLevel)

  -- Message
  msgEl <- UI.span #. "debug-message" # set text (T.unpack entry.deMessage)

  -- JSON context (collapsible) if present
  case entry.deContext of
    Nothing -> do
      void $ element el #+ [element timeEl, element levelEl, element msgEl]

    Just ctx -> do
      -- Create expandable context section
      let jsonText = TL.toStrict $ TLE.decodeUtf8 $ encode ctx
      contextEl <- UI.div #. "debug-context collapsed"
      toggleBtn <- UI.span #. "debug-toggle" # set text "▶ context"
      jsonEl <- UI.pre #. "debug-json" # set text (T.unpack jsonText)
        # set style [("display", "none")]

      -- Track expanded state with IORef
      expandedRef <- liftIO $ newIORef False

      -- Toggle visibility on click
      on UI.click toggleBtn $ const $ do
        isExpanded <- liftIO $ readIORef expandedRef
        if isExpanded
          then do
            void $ element jsonEl # set style [("display", "none")]
            void $ element toggleBtn # set text "▶ context"
            liftIO $ writeIORef expandedRef False
          else do
            void $ element jsonEl # set style [("display", "block")]
            void $ element toggleBtn # set text "▼ context"
            liftIO $ writeIORef expandedRef True

      void $ element contextEl #+ [element toggleBtn, element jsonEl]
      void $ element el #+ [element timeEl, element levelEl, element msgEl, element contextEl]

  pure el

-- | Handle for state inspector that allows external refresh
data StateInspectorHandle = StateInspectorHandle
  { sihElement    :: Element           -- ^ The container element
  , sihJsonEl     :: Element           -- ^ The JSON pre element
  , sihExpandedRef :: IORef Bool       -- ^ Whether currently expanded
  }

-- | Create a state inspector widget
--
-- Shows the raw game state as pretty-printed JSON. Collapsible by default.
-- Returns a handle that can be used to refresh the view when state changes.
stateInspector :: ToJSON state => GUIBridge state -> UI StateInspectorHandle
stateInspector bridge = do
  container <- UI.div #. "state-inspector"

  -- Title (clickable to toggle)
  titleEl <- UI.div #. "state-inspector-title" # set text "▶ RAW STATE (live)"

  -- JSON content (hidden by default)
  jsonEl <- UI.pre #. "state-json" # set style [("display", "none")]

  -- Initial content
  updateStateInspectorEl jsonEl bridge

  -- Track expanded state with IORef
  expandedRef <- liftIO $ newIORef False

  -- Toggle visibility on click
  on UI.click titleEl $ const $ do
    isExpanded <- liftIO $ readIORef expandedRef
    if isExpanded
      then do
        void $ element jsonEl # set style [("display", "none")]
        void $ element titleEl # set text "▶ RAW STATE (live)"
        liftIO $ writeIORef expandedRef False
      else do
        void $ element jsonEl # set style [("display", "block")]
        void $ element titleEl # set text "▼ RAW STATE (live)"
        -- Refresh content when expanding
        updateStateInspectorEl jsonEl bridge
        liftIO $ writeIORef expandedRef True

  void $ element container #+ [element titleEl, element jsonEl]
  pure $ StateInspectorHandle container jsonEl expandedRef

-- | Internal: Update the state inspector element with current state
updateStateInspectorEl :: ToJSON state => Element -> GUIBridge state -> UI ()
updateStateInspectorEl jsonEl bridge = do
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let jsonText = TL.toStrict $ TLE.decodeUtf8 $ encodePretty state
  void $ element jsonEl # set text (T.unpack jsonText)

-- | Refresh the state inspector if it's currently expanded
-- Call this from polling loop when state changes
refreshStateInspector :: ToJSON state => StateInspectorHandle -> GUIBridge state -> UI ()
refreshStateInspector handle bridge = do
  isExpanded <- liftIO $ readIORef handle.sihExpandedRef
  when isExpanded $
    updateStateInspectorEl handle.sihJsonEl bridge

-- | Create a loading overlay
--
-- Shows a spinner and message when LLM is active.
loadingOverlay :: Text -> UI Element
loadingOverlay message = do
  overlay <- UI.div #. "loading-overlay"
  spinner <- UI.div #. "spinner"
  msgEl <- UI.span #. "loading-text" # set text (T.unpack message)

  void $ element overlay #+ [element spinner, element msgEl]
  pure overlay

-- | Add a class to an element
withClass :: String -> UI Element -> UI Element
withClass cls el = el #. cls

-- | Add additional class to an element
--
-- Note: This reads the current class attribute using FFI and appends the new class.
addClass :: String -> Element -> UI Element
addClass cls el = do
  -- Use FFI to get current classes and add new one
  runFunction $ ffi "$(%1).addClass(%2)" el cls
  pure el

-- | Focus an input element
--
-- Useful for auto-focusing text inputs when they appear.
focusElement :: Element -> UI ()
focusElement el = runFunction $ ffi "$(%1).focus()" el
