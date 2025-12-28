-- | Dice widget for the DM GUI
--
-- Displays visual dice with outcome tier colors for FitD mechanics.
-- Includes full keyboard navigation for accessibility.
module DM.GUI.Widgets.Dice
  ( diceChoice
  , dieCard
  , dieFace
  , tierLabel
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.State (Position(..), OutcomeTier(..), calculateOutcome)
import DM.GUI.Theme (TierColor(..), tierColor)
import Tidepool.GUI.Core (GUIBridge, RequestResponse(..), safeSubmitResponse)

-- | Create a dice choice widget
--
-- Displays the available dice from the pool with their outcome tiers
-- based on the current position.
--
-- Features:
-- - Click to select
-- - Keyboard navigation: Tab between dice, Enter/Space to select
-- - Number keys (1-9) for quick selection
diceChoice
  :: GUIBridge state
  -> Text           -- ^ Prompt
  -> Position       -- ^ Current position (affects outcome tiers)
  -> [(Int, Int)]   -- ^ (die value, index) pairs
  -> UI Element
diceChoice bridge prompt pos diceWithIndices = do
  wrapper <- UI.div #. "dice-wrapper"

  promptEl <- UI.div #. "choice-prompt" # set text (T.unpack prompt)

  -- Container with keyboard handling
  diceContainer <- UI.div #. "dice-container"
    # set (attr "role") "listbox"
    # set (attr "aria-label") "Select a die"

  -- Create cards with display numbers (1-based for shortcuts)
  cards <- mapM (dieCard bridge pos) (zip [1..] diceWithIndices)
  void $ element diceContainer #+ map element cards

  -- Set up keyboard shortcuts on the container
  on UI.keydown diceContainer $ \code -> do
    -- Number keys 1-9 for quick selection
    when (code >= 49 && code <= 57) $ do
      let num = code - 48  -- Convert keycode to 1-9
      when (num <= length diceWithIndices) $ do
        let (_, idx) = diceWithIndices !! (num - 1)
        liftIO $ safeSubmitResponse bridge (ChoiceResponse idx)

  void $ element wrapper #+ [element promptEl, element diceContainer]
  pure wrapper

-- | Create a single die card
dieCard
  :: GUIBridge state
  -> Position         -- ^ Current position
  -> (Int, (Int, Int)) -- ^ (display number, (die value, index))
  -> UI Element
dieCard bridge pos (displayNum, (dieValue, idx)) = do
  let tier = calculateOutcome pos dieValue
      color = tierToColor tier

  card <- UI.div #. "dice-card"
    # set (attr "tabindex") "0"  -- Make focusable
    # set (attr "role") "option"
    # set (attr "aria-label") (T.unpack $ tierLabel tier <> " - die showing " <> T.pack (show dieValue))
    # set style [("border-color", T.unpack (tierColor color))]

  -- Keyboard shortcut hint
  numHint <- UI.span #. "dice-num" # set text (show displayNum <> ".")

  faceEl <- UI.span #. "die-face" # set text (T.unpack (dieFace dieValue))

  tierEl <- UI.span #. "tier-label"
    # set text (T.unpack (tierLabel tier))
    # set style [("color", T.unpack (tierColor color))]

  -- Click handler
  on UI.click card $ const $ liftIO $
    safeSubmitResponse bridge (ChoiceResponse idx)

  -- Keyboard handler (Enter or Space to select)
  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $  -- Enter or Space
      safeSubmitResponse bridge (ChoiceResponse idx)

  void $ element card #+ [element numHint, element faceEl, element tierEl]
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
