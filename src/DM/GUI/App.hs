-- | Main DM GUI application
--
-- This module provides the main layout and wiring for the DM Agent GUI.
-- It combines the generic Tidepool widgets with DM-specific widgets.
module DM.GUI.App
  ( -- * Application
    dmGUISetup
  , DMGUIConfig(..)
  , defaultDMGUIConfig
    -- * Polling
  , setupPolling
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (WorldState(..))
import DM.GUI.Theme (noirTheme)
import DM.GUI.Widgets.Stats (statsPanel)
import DM.GUI.Widgets.Mood (moodHeader)
import Tidepool.GUI.Core
import Tidepool.GUI.Theme (applyTheme)
import Tidepool.GUI.Widgets (textInput, choiceCards, narrativePane, debugPanel, loadingOverlay)

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

-- | Set up the DM GUI for a window
dmGUISetup
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> Window
  -> UI ()
dmGUISetup config bridge window = do
  -- Apply the noir theme
  applyTheme noirTheme window

  -- Build the layout
  root <- UI.div #. "tidepool-root"

  -- Left sidebar: stats
  sidebar <- statsPanel bridge
  void $ element sidebar #. "sidebar"

  -- Main panel
  mainPanel <- buildMainPanel config bridge

  -- Debug panel (optional)
  debugPanelEl <- if config.dcShowDebug
    then Just <$> debugPanel bridge
    else pure Nothing

  -- Assemble layout
  let children = [element sidebar, element mainPanel]
        ++ maybe [] (\d -> [element d]) debugPanelEl

  void $ element root #+ children
  void $ getBody window #+ [element root]

  -- Set up polling for state updates
  setupPolling config bridge window root

-- | Build the main content panel
buildMainPanel
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> UI Element
buildMainPanel _config bridge = do
  panel <- UI.div #. "main-panel"

  -- Mood header
  moodEl <- moodHeader bridge

  -- Tab bar
  tabBar <- buildTabBar

  -- Content area with narrative
  contentArea <- UI.div #. "content-area"
  narrativeEl <- narrativePane bridge
  void $ element contentArea #+ [element narrativeEl]

  -- Input area (initially hidden, shown when there's a pending request)
  inputArea <- UI.div #. "input-area"
    # set style [("display", "none")]

  void $ element panel #+
    [ element moodEl
    , element tabBar
    , element contentArea
    , element inputArea
    ]

  pure panel

-- | Build the tab bar
buildTabBar :: UI Element
buildTabBar = do
  tabBar <- UI.div #. "tab-bar"

  gameTab <- UI.div #. "tab active" # set text "Game"
  historyTab <- UI.div #. "tab" # set text "History"

  -- Tab switching logic (simplified for now)
  on UI.click gameTab $ const $ do
    void $ element gameTab #. "tab active"
    void $ element historyTab #. "tab"

  on UI.click historyTab $ const $ do
    void $ element gameTab #. "tab"
    void $ element historyTab #. "tab active"

  void $ element tabBar #+ [element gameTab, element historyTab]
  pure tabBar

-- | Set up polling for state updates
--
-- This creates a timer that periodically checks for:
-- - Pending input requests
-- - State changes
-- - Loading state
setupPolling
  :: DMGUIConfig
  -> GUIBridge WorldState
  -> Window
  -> Element
  -> UI ()
setupPolling config bridge window root = do
  timer <- UI.timer # set UI.interval (config.dcPollIntervalMs)

  -- Find the input area in the DOM
  inputArea <- UI.div #. "input-area-placeholder"

  on UI.tick timer $ const $ do
    -- Check for pending requests
    pending <- liftIO $ atomically $ readTVar (gbPendingRequest bridge)

    case pending of
      Nothing -> do
        -- Hide input area
        -- (In a real app, we'd find and update the actual input area element)
        pure ()

      Just (PendingText prompt) -> do
        -- Show text input
        inputWidget <- textInput bridge prompt
        -- Update the input area (simplified)
        pure ()

      Just (PendingChoice prompt options) -> do
        -- Show choice cards
        choicesWidget <- choiceCards bridge prompt options
        pure ()

    -- Check loading state
    loading <- liftIO $ atomically $ readTVar (gbLLMActive bridge)
    when loading $ do
      overlay <- loadingOverlay "The DM is thinking..."
      -- Show overlay (simplified)
      pure ()

  UI.start timer
