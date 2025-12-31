{-# LANGUAGE OverloadedStrings #-}
-- | Main Tidying GUI application
--
-- Provides a chat-centric layout for the Marie Kondo tidying agent.
-- Layout: header + chat area + input area + debug panel.
module Tidying.GUI.App
  ( -- * Application
    tidyingGUISetup
  , TidyingGUIConfig(..)
  , defaultTidyingGUIConfig
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void, when)
import Data.Aeson (ToJSON, fromJSON)
import Data.Aeson qualified as Aeson
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidying.State (Phase(..))
import Tidying.Question (Question)
import Tidying.GUI.Theme (tidyingTheme)
import Tidying.GUI.Widgets.Chat (chatPane, updateChatPane, typingIndicator)
import Tidying.GUI.Widgets.Input (textInputWithPhoto)
import Tidying.GUI.Widgets.Question (renderQuestion, QuestionResult(..))
import Tidepool.GUI.Core hiding (logDebug, logInfo, logWarn, logError)
import qualified Tidepool.GUI.Core as GUICore
import Tidepool.GUI.Theme (applyTheme)
import Tidepool.GUI.Widgets (choiceCards, updateDebugPanel, stateInspector, StateInspectorHandle(..), refreshStateInspector, focusElement)

-- | Configuration for the Tidying GUI
data TidyingGUIConfig = TidyingGUIConfig
  { tcShowDebug :: Bool
    -- ^ Whether to show the debug panel
  , tcPollIntervalMs :: Int
    -- ^ Polling interval for state updates (milliseconds)
  }
  deriving (Show, Eq)

-- | Default configuration
defaultTidyingGUIConfig :: TidyingGUIConfig
defaultTidyingGUIConfig = TidyingGUIConfig
  { tcShowDebug = True
  , tcPollIntervalMs = 100
  }

-- | Holds references to DOM elements that need dynamic updates
data GUIElements = GUIElements
  { geChatPane       :: Element  -- ^ Chat messages container
  , geInputArea      :: Element  -- ^ Input widget container
  , gePhaseLabel     :: Element  -- ^ Phase indicator in header
  , geLoadingEl      :: Element  -- ^ Typing indicator (hidden by default)
  , geDebugContent   :: Maybe Element  -- ^ Debug panel content (if shown)
  , geStateInspector :: Maybe StateInspectorHandle  -- ^ State inspector
  }

-- | Set up the Tidying GUI for a window
tidyingGUISetup
  :: ToJSON state
  => TidyingGUIConfig
  -> GUIBridge state
  -> (state -> Phase)  -- ^ Extract phase from state
  -> Window
  -> UI ()
tidyingGUISetup config bridge getPhase window = do
  -- Apply the tidying theme (calming colors)
  applyTheme tidyingTheme window

  -- Build the layout
  (root, elements) <- buildLayout config bridge getPhase

  void $ getBody window #+ [element root]

  -- Set up polling for state updates
  setupPolling config bridge getPhase elements

-- | Build the complete layout
buildLayout
  :: ToJSON state
  => TidyingGUIConfig
  -> GUIBridge state
  -> (state -> Phase)
  -> UI (Element, GUIElements)
buildLayout config bridge getPhase = do
  root <- UI.div #. "chat-layout"

  -- Main chat area
  mainPanel <- UI.div #. "chat-main"

  -- Header with phase indicator
  header <- UI.div #. "chat-header"
  titleEl <- UI.div #. "chat-header-title" # set text "TIDYING SESSION"
  phaseEl <- UI.div #. "chat-header-phase"

  -- Get initial phase
  state <- liftIO $ atomically $ readTVar bridge.gbState
  void $ element phaseEl # set text (T.unpack $ phaseText $ getPhase state)

  void $ element header #+ [element titleEl, element phaseEl]

  -- Chat pane (scrolling message area)
  chatEl <- chatPane
  updateChatPane chatEl bridge

  -- Typing indicator (hidden by default)
  loadingEl <- typingIndicator
  void $ element loadingEl # set style [("display", "none")]

  -- Input area (initially hidden)
  inputArea <- UI.div #. "chat-input-area"
    # set style [("display", "none")]

  void $ element mainPanel #+
    [ element header
    , element chatEl
    , element loadingEl
    , element inputArea
    ]

  -- Debug panel (optional)
  (debugPanelEl, debugContentEl, stateInspectorHandle) <- if config.tcShowDebug
    then do
      panel <- UI.div #. "debug-panel"
      titleEl' <- UI.div #. "debug-section-title" # set text "DEBUG LOG"
      contentEl <- UI.div #. "debug-content"
      inspectorHandle <- stateInspector bridge
      void $ element panel #+ [element titleEl', element contentEl, element inspectorHandle.sihElement]
      pure (Just panel, Just contentEl, Just inspectorHandle)
    else pure (Nothing, Nothing, Nothing)

  -- Assemble layout
  let children = [element mainPanel]
        ++ maybe [] (\d -> [element d]) debugPanelEl

  void $ element root #+ children

  let elements = GUIElements
        { geChatPane = chatEl
        , geInputArea = inputArea
        , gePhaseLabel = phaseEl
        , geLoadingEl = loadingEl
        , geDebugContent = debugContentEl
        , geStateInspector = stateInspectorHandle
        }

  pure (root, elements)

-- | Convert phase to display text
phaseText :: Phase -> Text
phaseText Surveying = "Surveying your space..."
phaseText Sorting = "Sorting items"
phaseText Splitting = "Breaking down categories"
phaseText Refining = "Refining decisions"
phaseText DecisionSupport = "Helping you decide"

-- | Set up polling for state updates
setupPolling
  :: ToJSON state
  => TidyingGUIConfig
  -> GUIBridge state
  -> (state -> Phase)
  -> GUIElements
  -> UI ()
setupPolling config bridge getPhase elements = do
  timer <- UI.timer # set UI.interval (config.tcPollIntervalMs)

  -- Track previous state
  prevRequestRef <- liftIO $ newIORef (Nothing :: Maybe PendingRequest)
  prevLoadingRef <- liftIO $ newIORef False
  prevNarrativeVersionRef <- liftIO $ newIORef (0 :: Int)
  prevStateVersionRef <- liftIO $ newIORef (0 :: Int)
  prevDebugVersionRef <- liftIO $ newIORef (0 :: Int)

  on UI.tick timer $ const $ do
    -- Check for pending requests
    pending <- liftIO $ atomically $ readTVar bridge.gbPendingRequest
    prevRequest <- liftIO $ readIORef prevRequestRef

    when (pending /= prevRequest) $ do
      liftIO $ writeIORef prevRequestRef pending
      updateInputArea bridge elements pending

    -- Check loading state
    loading <- liftIO $ atomically $ readTVar bridge.gbLLMActive
    prevLoading <- liftIO $ readIORef prevLoadingRef

    when (loading /= prevLoading) $ do
      liftIO $ writeIORef prevLoadingRef loading
      updateLoadingIndicator elements loading

    -- Check for narrative changes
    narrativeVersion <- liftIO $ atomically $ readTVar bridge.gbNarrativeVersion
    prevNarrativeVersion <- liftIO $ readIORef prevNarrativeVersionRef

    when (narrativeVersion /= prevNarrativeVersion) $ do
      liftIO $ writeIORef prevNarrativeVersionRef narrativeVersion
      updateChatPane elements.geChatPane bridge
      liftIO $ trimNarrativeLog bridge defaultMaxLogEntries

    -- Check for state changes (phase update)
    stateVersion <- liftIO $ atomically $ readTVar bridge.gbStateVersion
    prevStateVersion <- liftIO $ readIORef prevStateVersionRef

    when (stateVersion /= prevStateVersion) $ do
      liftIO $ writeIORef prevStateVersionRef stateVersion
      state <- liftIO $ atomically $ readTVar bridge.gbState
      void $ element elements.gePhaseLabel # set text (T.unpack $ phaseText $ getPhase state)
      -- Refresh state inspector if expanded
      case elements.geStateInspector of
        Just handle -> refreshStateInspector handle bridge
        Nothing -> pure ()

    -- Check for debug log changes
    debugVersion <- liftIO $ atomically $ readTVar bridge.gbDebugVersion
    prevDebugVersion <- liftIO $ readIORef prevDebugVersionRef

    when (debugVersion /= prevDebugVersion) $ do
      liftIO $ writeIORef prevDebugVersionRef debugVersion
      case elements.geDebugContent of
        Just contentEl -> updateDebugPanel contentEl bridge
        Nothing -> pure ()
      liftIO $ trimDebugLog bridge defaultMaxLogEntries

  UI.start timer

-- | Cleanup script to stop camera streams before destroying widget
--
-- This prevents orphaned streams when the input widget is swapped out.
cleanupCameraScript :: Text
cleanupCameraScript = T.unlines
  [ "(function() {"
  , "  window.cameraModalPending = false;"
  , "  if (window.cameraModalStream) {"
  , "    window.cameraModalStream.getTracks().forEach(function(t) { t.stop(); });"
  , "    window.cameraModalStream = null;"
  , "  }"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  if (video) video.srcObject = null;"
  , "})();"
  ]

-- | Update the input area based on pending request
updateInputArea :: GUIBridge state -> GUIElements -> Maybe PendingRequest -> UI ()
updateInputArea bridge elements pending = do
  let inputArea = elements.geInputArea

  -- Cleanup any active camera before swapping widgets
  runFunction $ ffi $ T.unpack cleanupCameraScript

  case pending of
    Nothing -> do
      -- Hide input area
      void $ element inputArea # set style [("display", "none")]
      void $ element inputArea # set children []

    Just (PendingText prompt) -> do
      -- Show text input with optional photo attachment
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "block")]
      inputWidget <- textInputWithPhoto bridge prompt
      void $ element inputArea #+ [element inputWidget]

    Just (PendingChoice prompt options) -> do
      -- Show choice cards
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "block")]
      choicesWidget <- choiceCards bridge prompt options
      void $ element inputArea #+ [element choicesWidget]

    Just (PendingPhoto _) -> do
      -- Photo-only upload not used in tidying (use textInputWithPhoto instead)
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "none")]

    Just (PendingDice _ _) -> do
      -- Dice selection not used in tidying, but handle gracefully
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "none")]

    Just PendingCharacterCreation -> do
      -- Character creation not used in tidying
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "none")]

    Just (PendingCustom "question" val) -> do
      -- Question DSL widget
      liftIO $ GUICore.logInfo bridge $ "GUI: Received PendingCustom question"
      liftIO $ GUICore.logDebug bridge $ "GUI: Question JSON: " <> T.pack (show val)
      case fromJSON val of
        Aeson.Success (q :: Question) -> do
          liftIO $ GUICore.logInfo bridge $ "GUI: Parsed question successfully, rendering..."
          void $ element inputArea # set children []
          void $ element inputArea # set style [("display", "block")]
          result <- renderQuestion bridge q
          void $ element inputArea #+ [element result.qrElement]
          liftIO $ GUICore.logInfo bridge "GUI: Question widget rendered"
          -- Auto-focus if there's a target
          case result.qrFocusTarget of
            Just el -> focusElement el
            Nothing -> pure ()
        Aeson.Error err -> do
          -- Failed to decode question, log and hide
          liftIO $ GUICore.logError bridge $ "GUI: Failed to decode question: " <> T.pack err
          void $ element inputArea # set style [("display", "none")]

    Just (PendingCustom _ _) -> do
      -- Unknown custom request type
      void $ element inputArea # set children []
      void $ element inputArea # set style [("display", "none")]

-- | Update the loading indicator visibility
updateLoadingIndicator :: GUIElements -> Bool -> UI ()
updateLoadingIndicator elements isLoading = do
  let loadingEl = elements.geLoadingEl
  if isLoading
    then void $ element loadingEl # set style [("display", "flex")]
    else void $ element loadingEl # set style [("display", "none")]
