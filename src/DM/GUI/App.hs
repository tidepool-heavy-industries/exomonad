-- | Main DM GUI application
--
-- This module provides the main layout and wiring for the DM Agent GUI.
-- It combines the generic Tidepool widgets with DM-specific widgets.
--
-- = Architecture Notes
--
-- The GUI uses a polling-based update model:
--
-- 1. A timer fires every N milliseconds
-- 2. On each tick, we check TVars for state changes
-- 3. If state changed, we update the relevant DOM elements
--
-- This is simpler than full FRP but requires careful management of
-- which elements need updating.
--
-- = Key Elements
--
-- * Stats sidebar - refreshes when WorldState changes
-- * Mood header - refreshes when DMMood changes
-- * Narrative pane - refreshes when narrative log changes
-- * Input area - dynamically shows text input or choice cards
-- * Loading overlay - shown when LLM is active
--
module DM.GUI.App
  ( -- * Application
    dmGUISetup
  , DMGUIConfig(..)
  , defaultDMGUIConfig
    -- Note: refresh functions are internal. External code should use:
    -- - updateState bridge f   -- for WorldState changes
    -- - addNarrative bridge t  -- for narrative additions
    -- The polling loop will detect changes and refresh automatically.
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (WorldState(..), PlayerState(..), Position(..), PendingOutcome(..))
import DM.GUI.Theme (noirTheme)
import DM.GUI.Widgets.Stats (statsPanel, stressBar, coinDisplay, heatBar, wantedPips, traumaList)
import DM.GUI.Widgets.Mood (moodDisplay)
import DM.GUI.Widgets.History (historyTab, updateHistoryTab)
import DM.GUI.Widgets.Narrative (dmNarrativePane, updateDMNarrative)
import DM.GUI.Widgets.Clocks (clocksPanel, updateClocksPanel)
import DM.GUI.Widgets.Dice (diceChoice)
import Tidepool.GUI.Core
import Tidepool.GUI.Theme (applyTheme)
import Tidepool.GUI.Widgets (textInput, choiceCards, loadingOverlay, updateDebugPanel, stateInspector, StateInspectorHandle(..), refreshStateInspector)

-- | Configuration for the DM GUI
data DMGUIConfig = DMGUIConfig
  { dcShowDebug :: Bool
    -- ^ Whether to show the debug panel
  , dcPollIntervalMs :: Int
    -- ^ Polling interval for state updates (milliseconds)
  }
  deriving (Show, Eq)

-- | Default DM GUI configuration
defaultDMGUIConfig :: DMGUIConfig
defaultDMGUIConfig = DMGUIConfig
  { dcShowDebug = True
  , dcPollIntervalMs = 100
  }

-- | Holds references to DOM elements that need dynamic updates
--
-- This allows the polling loop to update specific elements without
-- rebuilding the entire DOM tree.
data GUIElements = GUIElements
  { geStatsContainer   :: Element  -- ^ Stats sidebar content
  , geClocksContainer  :: Element  -- ^ Clocks panel content
  , geMoodLabel        :: Element  -- ^ Mood label text
  , geMoodDescription  :: Element  -- ^ Mood description text
  , geNarrativePane    :: Element  -- ^ Narrative log container
  , geHistoryPane      :: Element  -- ^ History tab content (hidden when not active)
  , geInputArea        :: Element  -- ^ Input widget container
  , geLoadingOverlay   :: Element  -- ^ Loading overlay (hidden by default)
  , geDebugContent     :: Maybe Element  -- ^ Debug panel content (if shown)
  , geStateInspector   :: Maybe StateInspectorHandle  -- ^ State inspector (if debug shown)
  , geGameTab          :: Element  -- ^ Game tab button
  , geHistoryTab       :: Element  -- ^ History tab button
  }

-- | Set up the DM GUI for a window
dmGUISetup
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> Window
  -> UI ()
dmGUISetup config bridge window = do
  -- Apply the noir theme
  applyTheme noirTheme window

  -- Build the layout with element references
  (root, elements) <- buildLayout config bridge

  void $ getBody window #+ [element root]

  -- Set up polling for state updates
  setupPolling config bridge elements

-- | Build the complete layout, returning references to updatable elements
buildLayout
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> UI (Element, GUIElements)
buildLayout config bridge = do
  root <- UI.div #. "tidepool-root"

  -- Left sidebar: stats + clocks
  sidebar <- UI.div #. "sidebar"
  statsContent <- statsPanel bridge
  clocksContent <- clocksPanel bridge
  void $ element sidebar #+ [element statsContent, element clocksContent]

  -- Main panel
  (mainPanel, moodLbl, moodDesc, narrativeEl, historyEl, inputEl, gameTab, histTab)
    <- buildMainPanel bridge

  -- Debug panel (optional) - create inline to capture content element
  (debugPanelEl, debugContentEl, stateInspectorHandle) <- if config.dcShowDebug
    then do
      panel <- UI.div #. "debug-panel"
      titleEl <- UI.div #. "debug-section-title" # set text "DEBUG LOG"
      contentEl <- UI.div #. "debug-content"
      -- State inspector (collapsible raw JSON view, live-updating)
      inspectorHandle <- stateInspector bridge
      void $ element panel #+ [element titleEl, element contentEl, element inspectorHandle.sihElement]
      pure (Just panel, Just contentEl, Just inspectorHandle)
    else pure (Nothing, Nothing, Nothing)

  -- Loading overlay (hidden by default)
  loadingEl <- loadingOverlay "The DM is thinking..."
  void $ element loadingEl # set style [("display", "none")]

  -- Assemble layout
  let panelChildren = [element sidebar, element mainPanel]
        ++ maybe [] (\d -> [element d]) debugPanelEl

  void $ element root #+ (panelChildren ++ [element loadingEl])

  let elements = GUIElements
        { geStatsContainer = statsContent
        , geClocksContainer = clocksContent
        , geMoodLabel = moodLbl
        , geMoodDescription = moodDesc
        , geNarrativePane = narrativeEl
        , geHistoryPane = historyEl
        , geInputArea = inputEl
        , geLoadingOverlay = loadingEl
        , geDebugContent = debugContentEl
        , geStateInspector = stateInspectorHandle
        , geGameTab = gameTab
        , geHistoryTab = histTab
        }

  pure (root, elements)

-- | Build the main content panel, returning references to updatable elements
buildMainPanel
  :: GUIBridge WorldState
  -> UI (Element, Element, Element, Element, Element, Element, Element, Element)
buildMainPanel bridge = do
  panel <- UI.div #. "main-panel"

  -- Mood header with separate label and description for updates
  moodHeaderEl <- UI.div #. "mood-header"
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let (lbl, desc) = moodDisplay state.mood
  moodLabelEl <- UI.span #. "mood-label" # set text (T.unpack lbl)
  moodDescEl <- UI.span #. "mood-description" # set text (T.unpack desc)
  void $ element moodHeaderEl #+ [element moodLabelEl, UI.string " - ", element moodDescEl]

  -- Tab bar with references to tabs
  (tabBar, gameTab, historyTabEl) <- buildTabBar

  -- Content area - holds BOTH narrative and history (show/hide for tab switching)
  contentArea <- UI.div #. "content-area"

  -- Narrative pane (visible by default) - tabpanel for Game tab
  narrativeEl <- dmNarrativePane bridge
  void $ element narrativeEl
    # set (attr "role") "tabpanel"
    # set (attr "aria-label") "Game narrative"

  -- History pane (hidden by default) - tabpanel for History tab
  historyEl <- historyTab bridge
  void $ element historyEl
    # set (attr "role") "tabpanel"
    # set (attr "aria-label") "Session history"
    # set style [("display", "none")]

  void $ element contentArea #+ [element narrativeEl, element historyEl]

  -- Input area (initially hidden)
  inputArea <- UI.div #. "input-area"
    # set style [("display", "none")]

  void $ element panel #+
    [ element moodHeaderEl
    , element tabBar
    , element contentArea
    , element inputArea
    ]

  pure (panel, moodLabelEl, moodDescEl, narrativeEl, historyEl, inputArea, gameTab, historyTabEl)

-- | Build the tab bar, returning references to tab elements
--
-- Implements WAI-ARIA tabs pattern for accessibility:
-- - Tab bar has role="tablist"
-- - Each tab has role="tab", tabindex, and aria-selected
-- - Keyboard: Enter/Space to activate
buildTabBar :: UI (Element, Element, Element)
buildTabBar = do
  tabBar <- UI.div #. "tab-bar"
    # set (attr "role") "tablist"

  gameTab <- UI.div #. "tab active"
    # set text "Game"
    # set (attr "role") "tab"
    # set (attr "tabindex") "0"
    # set (attr "aria-selected") "true"

  historyTab' <- UI.div #. "tab"
    # set text "History"
    # set (attr "role") "tab"
    # set (attr "tabindex") "0"
    # set (attr "aria-selected") "false"

  void $ element tabBar #+ [element gameTab, element historyTab']
  pure (tabBar, gameTab, historyTab')

-- | Which tab is currently active
data TabType = GameTab | HistoryTab
  deriving (Eq)

-- | Set up polling for state updates
--
-- This creates a timer that periodically checks for:
-- - Pending input requests (show/hide input widgets)
-- - Loading state (show/hide overlay)
-- - Game state changes (refresh stats, mood)
-- - Narrative changes (refresh narrative pane)
--
-- Uses version numbers for efficient change detection.
setupPolling
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> GUIElements
  -> UI ()
setupPolling config bridge elements = do
  timer <- UI.timer # set UI.interval (config.dcPollIntervalMs)

  -- Track previous state to avoid unnecessary DOM updates
  prevRequestRef <- liftIO $ newIORef (Nothing :: Maybe PendingRequest)
  prevLoadingRef <- liftIO $ newIORef False
  currentTabRef <- liftIO $ newIORef GameTab

  -- Track state versions for efficient change detection
  prevStateVersionRef <- liftIO $ newIORef (0 :: Int)
  prevNarrativeVersionRef <- liftIO $ newIORef (0 :: Int)
  prevDebugVersionRef <- liftIO $ newIORef (0 :: Int)

  -- Set up tab click handlers
  on UI.click elements.geGameTab $ const $ do
    currentTab <- liftIO $ readIORef currentTabRef
    when (currentTab /= GameTab) $ do
      liftIO $ writeIORef currentTabRef GameTab
      switchToGameTab elements

  on UI.click elements.geHistoryTab $ const $ do
    currentTab <- liftIO $ readIORef currentTabRef
    when (currentTab /= HistoryTab) $ do
      liftIO $ writeIORef currentTabRef HistoryTab
      switchToHistoryTab elements

  -- Keyboard handlers for tabs (Enter or Space to activate)
  on UI.keydown elements.geGameTab $ \code ->
    when (code == 13 || code == 32) $ do  -- Enter or Space
      currentTab <- liftIO $ readIORef currentTabRef
      when (currentTab /= GameTab) $ do
        liftIO $ writeIORef currentTabRef GameTab
        switchToGameTab elements

  on UI.keydown elements.geHistoryTab $ \code ->
    when (code == 13 || code == 32) $ do  -- Enter or Space
      currentTab <- liftIO $ readIORef currentTabRef
      when (currentTab /= HistoryTab) $ do
        liftIO $ writeIORef currentTabRef HistoryTab
        switchToHistoryTab elements

  on UI.tick timer $ const $ do
    -- Check for pending requests
    pending <- liftIO $ atomically $ readTVar bridge.gbPendingRequest
    prevRequest <- liftIO $ readIORef prevRequestRef

    -- Only update input area if request changed
    when (pending /= prevRequest) $ do
      liftIO $ writeIORef prevRequestRef pending
      updateInputArea bridge elements pending

    -- Check loading state
    loading <- liftIO $ atomically $ readTVar bridge.gbLLMActive
    prevLoading <- liftIO $ readIORef prevLoadingRef

    when (loading /= prevLoading) $ do
      liftIO $ writeIORef prevLoadingRef loading
      updateLoadingOverlay elements loading

    -- Check for game state changes (stats, mood)
    stateVersion <- liftIO $ atomically $ readTVar bridge.gbStateVersion
    prevStateVersion <- liftIO $ readIORef prevStateVersionRef

    when (stateVersion /= prevStateVersion) $ do
      liftIO $ writeIORef prevStateVersionRef stateVersion
      -- Refresh all state-dependent widgets
      refreshStats bridge elements
      refreshMood bridge elements
      refreshClocks bridge elements
      refreshHistory bridge elements
      -- Refresh state inspector if it's expanded (live view)
      case elements.geStateInspector of
        Just handle -> refreshStateInspector handle bridge
        Nothing -> pure ()

    -- Check for narrative changes
    narrativeVersion <- liftIO $ atomically $ readTVar bridge.gbNarrativeVersion
    prevNarrativeVersion <- liftIO $ readIORef prevNarrativeVersionRef

    when (narrativeVersion /= prevNarrativeVersion) $ do
      liftIO $ writeIORef prevNarrativeVersionRef narrativeVersion
      currentTab <- liftIO $ readIORef currentTabRef
      -- Only refresh narrative if on Game tab
      when (currentTab == GameTab) $
        refreshNarrative bridge elements
      -- Trim narrative log to prevent unbounded growth
      liftIO $ trimNarrativeLog bridge defaultMaxLogEntries

    -- Check for debug log changes
    debugVersion <- liftIO $ atomically $ readTVar bridge.gbDebugVersion
    prevDebugVersion <- liftIO $ readIORef prevDebugVersionRef

    when (debugVersion /= prevDebugVersion) $ do
      liftIO $ writeIORef prevDebugVersionRef debugVersion
      refreshDebug bridge elements
      -- Trim debug log to prevent unbounded growth
      liftIO $ trimDebugLog bridge defaultMaxLogEntries

  UI.start timer

-- | Update the input area based on pending request
updateInputArea :: GUIBridge WorldState -> GUIElements -> Maybe PendingRequest -> UI ()
updateInputArea bridge elements pending = do
  let inputArea = elements.geInputArea

  case pending of
    Nothing -> do
      -- Hide input area and clear contents
      void $ element inputArea # set style [("display", "none")]
      void $ element inputArea # set children []

    Just (PendingText prompt) -> do
      -- Clear and show text input
      void $ element inputArea # set children []
      inputWidget <- textInput bridge prompt
      void $ element inputArea #+ [element inputWidget]
      void $ element inputArea # set style [("display", "block")]

    Just (PendingChoice prompt options) -> do
      -- Clear and show choice cards
      void $ element inputArea # set children []
      choicesWidget <- choiceCards bridge prompt options
      void $ element inputArea #+ [element choicesWidget]
      void $ element inputArea # set style [("display", "block")]

    Just (PendingDice prompt diceWithIndices) -> do
      -- Get current position from state for tier calculation
      state <- liftIO $ atomically $ readTVar bridge.gbState
      let pos = getCurrentPosition state

      -- Clear and show dice selection
      void $ element inputArea # set children []
      diceWidget <- diceChoice bridge prompt pos diceWithIndices
      void $ element inputArea #+ [element diceWidget]
      void $ element inputArea # set style [("display", "block")]

-- | Get the current position from world state
--
-- Defaults to Risky if no pending outcome is set.
getCurrentPosition :: WorldState -> Position
getCurrentPosition state =
  case state.pendingOutcome of
    Just po -> po.outcomePosition
    Nothing -> Risky  -- Default fallback

-- | Update the loading overlay visibility
updateLoadingOverlay :: GUIElements -> Bool -> UI ()
updateLoadingOverlay elements isLoading = do
  let overlay = elements.geLoadingOverlay
  if isLoading
    then void $ element overlay # set style [("display", "flex")]
    else void $ element overlay # set style [("display", "none")]

-- | Switch to the Game tab
switchToGameTab :: GUIElements -> UI ()
switchToGameTab elements = do
  -- Update tab styling and ARIA state
  void $ element elements.geGameTab #. "tab active"
    # set (attr "aria-selected") "true"
  void $ element elements.geHistoryTab #. "tab"
    # set (attr "aria-selected") "false"

  -- Show narrative, hide history
  void $ element elements.geNarrativePane # set style [("display", "block")]
  void $ element elements.geHistoryPane # set style [("display", "none")]

-- | Switch to the History tab
switchToHistoryTab :: GUIElements -> UI ()
switchToHistoryTab elements = do
  -- Update tab styling and ARIA state
  void $ element elements.geGameTab #. "tab"
    # set (attr "aria-selected") "false"
  void $ element elements.geHistoryTab #. "tab active"
    # set (attr "aria-selected") "true"

  -- Hide narrative, show history
  void $ element elements.geNarrativePane # set style [("display", "none")]
  void $ element elements.geHistoryPane # set style [("display", "block")]

-- | Refresh stats display (call when WorldState changes)
refreshStats :: GUIBridge WorldState -> GUIElements -> UI ()
refreshStats bridge elements = do
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let player = state.player

  -- Rebuild stats content
  void $ element elements.geStatsContainer # set children []

  titleEl <- UI.h2 #. "sidebar-title" # set text "SCOUNDREL"
  stressEl <- stressBar player.stress
  coinEl <- coinDisplay player.coin
  heatEl <- heatBar player.heat
  wantedEl <- wantedPips player.wanted
  traumaEl <- traumaList player.trauma

  void $ element elements.geStatsContainer #+
    [ element titleEl
    , element stressEl
    , element coinEl
    , element heatEl
    , element wantedEl
    , element traumaEl
    ]

-- | Refresh narrative pane (call when WorldState.scene changes)
refreshNarrative :: GUIBridge WorldState -> GUIElements -> UI ()
refreshNarrative bridge elements =
  -- Use DM-specific update for rich narrative with speech bubbles
  updateDMNarrative elements.geNarrativePane bridge

-- | Refresh mood display (call when mood changes)
refreshMood :: GUIBridge WorldState -> GUIElements -> UI ()
refreshMood bridge elements = do
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let (lbl, desc) = moodDisplay state.mood

  void $ element elements.geMoodLabel # set text (T.unpack lbl)
  void $ element elements.geMoodDescription # set text (T.unpack desc)

-- | Refresh clocks panel (call when WorldState.clocks changes)
refreshClocks :: GUIBridge WorldState -> GUIElements -> UI ()
refreshClocks bridge elements =
  updateClocksPanel elements.geClocksContainer bridge

-- | Refresh history pane (call when WorldState changes)
refreshHistory :: GUIBridge WorldState -> GUIElements -> UI ()
refreshHistory bridge elements =
  updateHistoryTab elements.geHistoryPane bridge

-- | Refresh debug panel (call when debug log changes)
refreshDebug :: GUIBridge WorldState -> GUIElements -> UI ()
refreshDebug bridge elements =
  case elements.geDebugContent of
    Nothing -> pure ()  -- Debug panel not shown
    Just contentEl -> updateDebugPanel contentEl bridge
