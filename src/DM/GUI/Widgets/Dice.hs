-- | Dice widget for the DM GUI
--
-- Displays visual dice with outcome tier colors for FitD mechanics.
module DM.GUI.Widgets.Dice
  ( diceChoice
  , dieCard
  , dieFace
  , tierLabel
  ) where

import Control.Concurrent.MVar (putMVar)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (Position(..), OutcomeTier(..), calculateOutcome)
import DM.GUI.Theme (TierColor(..), tierColor)
import Tidepool.GUI.Core

-- | Create a dice choice widget
--
-- Displays the available dice from the pool with their outcome tiers
-- based on the current position.
diceChoice
  :: GUIBridge state
  -> Text           -- ^ Prompt
  -> Position       -- ^ Current position (affects outcome tiers)
  -> [Int]          -- ^ Available dice values
  -> UI Element
diceChoice bridge prompt pos dice = do
  container <- UI.div #. "dice-container"

  promptEl <- UI.div #. "choice-prompt" # set text (T.unpack prompt)

  cards <- mapM (dieCard bridge pos) (zip [0..] dice)

  void $ element container #+ (element promptEl : map element cards)
  pure container

-- | Create a single die card
dieCard
  :: GUIBridge state
  -> Position       -- ^ Current position
  -> (Int, Int)     -- ^ (index, die value)
  -> UI Element
dieCard bridge pos (idx, dieValue) = do
  let tier = calculateOutcome pos dieValue
      color = tierToColor tier

  card <- UI.div #. "dice-card"
    # set style [("border-color", T.unpack (tierColor color))]

  faceEl <- UI.span #. "die-face" # set text (T.unpack (dieFace dieValue))

  tierEl <- UI.span #. "tier-label"
    # set text (T.unpack (tierLabel tier))
    # set style [("color", T.unpack (tierColor color))]

  on UI.click card $ const $ liftIO $
    putMVar (gbRequestResponse bridge) (ChoiceResponse idx)

  void $ element card #+ [element faceEl, element tierEl]
  pure card

-- | Get the Unicode die face for a value
dieFace :: Int -> Text
dieFace 1 = "\x2680"  -- ⚀
dieFace 2 = "\x2681"  -- ⚁
dieFace 3 = "\x2682"  -- ⚂
dieFace 4 = "\x2683"  -- ⚃
dieFace 5 = "\x2684"  -- ⚄
dieFace 6 = "\x2685"  -- ⚅
dieFace _ = "?"

-- | Get the display label for an outcome tier
tierLabel :: OutcomeTier -> Text
tierLabel Critical = "CRITICAL!"
tierLabel Success  = "Success"
tierLabel Partial  = "Partial"
tierLabel Bad      = "Bad"
tierLabel Disaster = "DISASTER"

-- | Map outcome tier to theme color
tierToColor :: OutcomeTier -> TierColor
tierToColor Critical = TierCritical
tierToColor Success  = TierSuccess
tierToColor Partial  = TierPartial
tierToColor Bad      = TierBad
tierToColor Disaster = TierDisaster
