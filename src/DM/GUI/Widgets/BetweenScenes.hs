-- | BetweenScenes widget for the DM GUI
--
-- Displays the between-scenes context:
-- 1. Clock status panel (all visible clocks)
-- 2. Transition narration text
--
-- Option cards are rendered separately via the standard PendingChoice handling.
module DM.GUI.Widgets.BetweenScenes
  ( betweenScenesContextWidget
  , clockSummaryWidget
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (BetweenScenesContext(..), ClockSummary(..))

-- | Create the BetweenScenes context widget
--
-- Shows clocks and transition narration. Option cards are rendered
-- separately by the standard choice card mechanism.
betweenScenesContextWidget :: BetweenScenesContext -> UI Element
betweenScenesContextWidget ctx = do
  wrapper <- UI.div #. "between-scenes-context"

  -- Header
  header <- UI.h2 #. "between-scenes-header" # set text "Between Scenes"

  -- Clocks panel
  clocksPanel <- UI.div #. "between-scenes-clocks"
  clocksTitle <- UI.div #. "clocks-title" # set text "The Clocks Tick..."
  clockWidgets <- mapM clockSummaryWidget ctx.bscClocks
  void $ element clocksPanel #+ (element clocksTitle : map element clockWidgets)

  -- Transition narration
  narration <- UI.div #. "between-scenes-narration"
    # set text (T.unpack ctx.bscTransitionNarration)

  void $ element wrapper #+
    [ element header
    , element clocksPanel
    , element narration
    ]

  pure wrapper

-- | Create a clock summary widget
clockSummaryWidget :: ClockSummary -> UI Element
clockSummaryWidget cs = do
  widget <- UI.div #. ("clock-summary " <> if cs.csIsThreat then "threat" else "goal")

  nameEl <- UI.span #. "clock-summary-name" # set text (T.unpack cs.csName)

  -- Simple bar representation
  let bar = T.concat
        [ T.replicate cs.csFilled (if cs.csIsThreat then "\x25CF" else "\x25C9")  -- filled
        , T.replicate (cs.csSegments - cs.csFilled) "\x25CB"  -- empty
        ]
  barEl <- UI.span #. "clock-summary-bar" # set text (T.unpack bar)

  void $ element widget #+ [element nameEl, element barEl]
  pure widget
