-- | Stats panel widget for the DM GUI
--
-- Displays player stats: stress, coin, heat, wanted level, and trauma.
module DM.GUI.Widgets.Stats
  ( statsPanel
  , stressBar
  , heatBar
  , wantedPips
  , coinDisplay
  , traumaList
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (WorldState(..), PlayerState(..), Trauma(..))
import Tidepool.GUI.Core (GUIBridge(..))

-- | Create the full stats panel
statsPanel :: GUIBridge WorldState -> UI Element
statsPanel bridge = do
  panel <- UI.div #. "stats-sidebar"

  -- Title
  title <- UI.h2 #. "sidebar-title" # set text "SCOUNDREL"

  -- Get current state
  state <- liftIO $ atomically $ readTVar bridge.gbState
  let player = state.player

  -- Stress bar (0-9)
  stressGroup <- stressBar player.stress

  -- Coin display
  coinGroup <- coinDisplay player.coin

  -- Heat bar (0-10)
  heatGroup <- heatBar player.heat

  -- Wanted pips (0-4)
  wantedGroup <- wantedPips player.wanted

  -- Trauma list
  traumaGroup <- traumaList player.trauma

  void $ element panel #+
    [ element title
    , element stressGroup
    , element coinGroup
    , element heatGroup
    , element wantedGroup
    , element traumaGroup
    ]

  pure panel

-- | Create a stress bar (0-9 segments)
stressBar :: Int -> UI Element
stressBar current = do
  group <- UI.div #. "stat-group"

  label <- UI.div #. "stat-label" # set text "Stress"
  bar <- UI.div #. "stat-bar"

  segments <- mapM (mkSegment current) [1..9]
  void $ element bar #+ map element segments

  void $ element group #+ [element label, element bar]
  pure group

-- | Create a heat bar (0-10 segments)
heatBar :: Int -> UI Element
heatBar current = do
  group <- UI.div #. "stat-group"

  label <- UI.div #. "stat-label" # set text "Heat"
  bar <- UI.div #. "stat-bar"

  segments <- mapM (mkSegment current) [1..10]
  void $ element bar #+ map element segments

  void $ element group #+ [element label, element bar]
  pure group

-- | Create a segment for a bar
mkSegment :: Int -> Int -> UI Element
mkSegment current idx = do
  let cls = if idx <= current then "stat-segment filled" else "stat-segment empty"
  UI.div #. cls

-- | Create wanted level pips (0-4)
wantedPips :: Int -> UI Element
wantedPips current = do
  group <- UI.div #. "stat-group"

  label <- UI.div #. "stat-label" # set text "Wanted"
  container <- UI.div #. "pip-container"

  pips <- mapM (mkPip current) [1..4]
  void $ element container #+ map element pips

  void $ element group #+ [element label, element container]
  pure group

-- | Create a pip
mkPip :: Int -> Int -> UI Element
mkPip current idx = do
  let cls = if idx <= current then "pip filled" else "pip empty"
  UI.div #. cls

-- | Create a coin display
coinDisplay :: Int -> UI Element
coinDisplay amount = do
  group <- UI.div #. "stat-group"

  label <- UI.div #. "stat-label" # set text "Coin"
  valueEl <- UI.div #. "stat-value" # set text (show amount)

  void $ element group #+ [element label, element valueEl]
  pure group

-- | Create a trauma list
traumaList :: [Trauma] -> UI Element
traumaList traumas = do
  group <- UI.div #. "stat-group"

  label <- UI.div #. "stat-label" # set text "Trauma"
  container <- UI.div #. "trauma-list"

  tags <- mapM mkTraumaTag traumas
  void $ element container #+ map element tags

  void $ element group #+ [element label, element container]
  pure group

-- | Create a trauma tag
mkTraumaTag :: Trauma -> UI Element
mkTraumaTag (Trauma txt) =
  UI.span #. "trauma-tag" # set text (T.unpack txt)
