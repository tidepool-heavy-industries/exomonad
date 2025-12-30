-- | Clocks widget for the DM GUI
--
-- Displays FitD-style progress clocks with hover tooltips.
module DM.GUI.Widgets.Clocks
  ( clocksPanel
  , updateClocksPanel
  , clockWidget
  , clockFace
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (WorldState(..), Clock(..), ClockId(..))
import Tidepool.GUI.Core (GUIBridge(..))

-- | Create a panel showing all visible clocks
clocksPanel :: GUIBridge WorldState -> UI Element
clocksPanel bridge = do
  panel <- UI.div #. "clocks-panel"
  updateClocksPanel panel bridge
  pure panel

-- | Update the clocks panel with current clock state
updateClocksPanel :: Element -> GUIBridge WorldState -> UI ()
updateClocksPanel panel bridge = do
  -- Clear existing content
  void $ element panel # set children []

  title <- UI.div #. "stat-label" # set text "Clocks"

  -- Get current state
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let visibleClocks = filter (.clockVisible) (HM.elems state.clocks)

  clockWidgets <- mapM clockWidget visibleClocks

  void $ element panel #+ (element title : map element clockWidgets)

-- | Create a single clock widget with hover tooltip
clockWidget :: Clock -> UI Element
clockWidget clock = do
  container <- UI.div #. "clock-widget"

  -- Clock face (SVG-style circle segments)
  face <- clockFace clock.clockSegments clock.clockFilled
  nameEl <- UI.span #. "clock-name" # set text (T.unpack clock.clockName)

  -- Tooltip (hidden by default, shown on hover)
  tooltip <- UI.div #. "clock-tooltip"
    # set style [("display", "none")]
    # set text (T.unpack ("When filled: " <> clock.clockConsequence))

  -- Hover events
  on UI.hover container $ const $
    void $ element tooltip # set style [("display", "block")]
  on UI.leave container $ const $
    void $ element tooltip # set style [("display", "none")]

  void $ element container #+ [element face, element nameEl, element tooltip]
  pure container

-- | Create a clock face with filled/empty segments
--
-- For now, uses a simple text representation. A real implementation
-- would use SVG for pie-chart style display.
clockFace :: Int -> Int -> UI Element
clockFace segments filled = do
  face <- UI.div #. "clock-face"

  -- Simple bar representation for now
  -- TODO: Replace with SVG pie chart
  let bar = T.concat
        [ T.replicate filled "●"
        , T.replicate (segments - filled) "○"
        ]

  void $ element face # set text (T.unpack bar)
  pure face

