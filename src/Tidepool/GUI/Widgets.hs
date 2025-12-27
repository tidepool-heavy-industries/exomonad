-- | Generic GUI widgets for Tidepool
--
-- These widgets can be used by any Tidepool-based game. Domain-specific
-- widgets (like dice with tier colors) are in their respective modules.
module Tidepool.GUI.Widgets
  ( -- * Input widgets
    textInput
  , choiceCards
    -- * Display widgets
  , narrativePane
  , debugPanel
    -- * Loading state
  , loadingOverlay
    -- * Layout helpers
  , withClass
  , addClass
  ) where

import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM (atomically, readTVar)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core

-- | Create a text input widget
--
-- When the user submits (Enter key or button click), the response
-- is put into the bridge's MVar.
textInput
  :: GUIBridge state
  -> Text           -- ^ Prompt/placeholder
  -> UI Element
textInput bridge prompt = do
  container <- UI.div #. "text-input-container"

  inputEl <- UI.input #. "text-input"
    # set (attr "placeholder") (T.unpack prompt)

  submitBtn <- UI.button #. "submit-btn" # set text "Submit"

  let submit = do
        val <- get value inputEl
        when (not (null val)) $ liftIO $ do
          putMVar (gbRequestResponse bridge) (TextResponse (T.pack val))
        void $ element inputEl # set value ""

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element container #+ [element inputEl, element submitBtn]
  pure container

-- | Create choice cards widget
--
-- Renders a set of clickable cards. When clicked, the response
-- is put into the bridge's MVar.
choiceCards
  :: GUIBridge state
  -> Text                -- ^ Prompt
  -> [(Text, Int)]       -- ^ (label, index) pairs
  -> UI Element
choiceCards bridge prompt options = do
  container <- UI.div #. "choice-container"

  promptEl <- UI.div #. "choice-prompt" # set text (T.unpack prompt)

  cards <- mapM (mkCard bridge) options

  void $ element container #+ (element promptEl : map element cards)
  pure container

mkCard :: GUIBridge state -> (Text, Int) -> UI Element
mkCard bridge (label, idx) = do
  card <- UI.div #. "choice-card"
  labelEl <- UI.span # set text (T.unpack label)

  on UI.click card $ const $ liftIO $
    putMVar (gbRequestResponse bridge) (ChoiceResponse idx)

  void $ element card #+ [element labelEl]
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
  entries <- liftIO $ atomically $ readTVar (gbNarrativeLog bridge)
  let entryEls = map mkNarrativeEntry (toList entries)
  void $ element container # set children []
  void $ element container #+ entryEls
  -- Auto-scroll to bottom
  runFunction $ ffi "$(%1).scrollTop($(%1)[0].scrollHeight)" container

mkNarrativeEntry :: Text -> UI Element
mkNarrativeEntry txt =
  UI.div #. "narrative-entry" # set text (T.unpack txt)

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
  entries <- liftIO $ atomically $ readTVar (gbDebugLog bridge)
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

  levelEl <- UI.span #. ("debug-level " <> levelClass)
    # set text (show entry.deLevel)
  msgEl <- UI.span # set text (T.unpack entry.deMessage)

  void $ element el #+ [element levelEl, element msgEl]
  pure el

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
addClass :: String -> Element -> UI Element
addClass cls el = do
  existing <- get (attr "class") el
  element el # set (attr "class") (existing ++ " " ++ cls)
