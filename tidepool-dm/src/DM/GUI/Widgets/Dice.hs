-- | Dice widget for the DM GUI
--
-- Displays visual dice with outcome tier colors for FitD mechanics.
-- Includes full keyboard navigation for accessibility.
module DM.GUI.Widgets.Dice
  ( diceChoice
  , dieCard
  , dieFaceClass
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
-- based on the current position. Each die shows an LLM-generated hint
-- about what that specific outcome would mean narratively.
--
-- Features:
-- - Click to select
-- - Keyboard navigation: Tab between dice, Enter/Space to select
-- - Number keys (1-9) for quick selection
-- - Position indicator showing current risk level
-- - LLM-generated hints for each die showing the specific outcome
diceChoice
  :: GUIBridge state
  -> Text           -- ^ Prompt
  -> Position       -- ^ Current position (affects outcome tiers)
  -> [(Int, Int, Text)]   -- ^ (die value, index, hint) triples
  -> UI Element
diceChoice bridge prompt pos diceWithHints = do
  wrapper <- UI.div #. "dice-wrapper"

  promptEl <- UI.div #. "choice-prompt" # set text (T.unpack prompt)

  -- Position indicator with explanation
  positionEl <- positionIndicator pos

  -- Container with keyboard handling
  diceContainer <- UI.div #. "dice-container"
    # set (attr "role") "listbox"
    # set (attr "aria-label") "Select a die"

  -- Create cards with display numbers (1-based for shortcuts)
  -- Each card shows the die, tier, and LLM-generated hint
  cards <- mapM (dieCard bridge pos) (zip [1..] diceWithHints)
  void $ element diceContainer #+ map element cards

  -- Set up keyboard shortcuts on the container
  on UI.keydown diceContainer $ \code -> do
    -- Number keys 1-9 for quick selection
    when (code >= 49 && code <= 57) $ do
      let num = code - 48  -- Convert keycode to 1-9
      when (num <= length diceWithHints) $ do
        let (_, idx, _) = diceWithHints !! (num - 1)
        liftIO $ safeSubmitResponse bridge (ChoiceResponse idx)

  -- Tier hints legend (static reference, less prominent now that cards have hints)
  hintsEl <- tierHintsLegend pos

  void $ element wrapper #+ [element promptEl, element positionEl, element diceContainer, element hintsEl]
  pure wrapper

-- | Position indicator showing current risk level
positionIndicator :: Position -> UI Element
positionIndicator pos = do
  let (posName, posClass, posDesc) = case pos of
        Controlled -> ("CONTROLLED", "position-controlled",
          "You have leverage. Failure means a lesser consequence.")
        Risky -> ("RISKY", "position-risky",
          "Standard situation. Failure means trouble.")
        Desperate -> ("DESPERATE", "position-desperate",
          "Against the odds. Failure means serious harm.")

  indicator <- UI.div #. ("position-indicator " <> posClass)

  badge <- UI.span #. "position-badge" # set text (T.unpack posName)
  desc <- UI.span #. "position-desc" # set text (T.unpack posDesc)

  void $ element indicator #+ [element badge, element desc]
  pure indicator

-- | Tier hints legend showing what each outcome means
tierHintsLegend :: Position -> UI Element
tierHintsLegend pos = do
  legend <- UI.div #. "tier-hints-legend"

  -- Show relevant tiers based on position
  let hints = case pos of
        Controlled ->
          [ (TierCritical, "6,6", "Critical", "Exceptional success + bonus")
          , (TierSuccess, "6", "Success", "You do it")
          , (TierPartial, "4-5", "Partial", "Succeed with minor cost")
          , (TierBad, "1-3", "Bad", "Reduced effect or cost")
          ]
        Risky ->
          [ (TierCritical, "6,6", "Critical", "Exceptional success + bonus")
          , (TierSuccess, "6", "Success", "You do it")
          , (TierPartial, "4-5", "Partial", "Succeed with complication")
          , (TierBad, "1-3", "Bad", "Things go wrong")
          ]
        Desperate ->
          [ (TierCritical, "6,6", "Critical", "Against all odds + bonus")
          , (TierSuccess, "6", "Success", "Barely made it")
          , (TierPartial, "4-5", "Partial", "Pyrrhic victory")
          , (TierDisaster, "1-3", "Disaster", "Catastrophic failure")
          ]

  hintEls <- mapM tierHintItem hints
  void $ element legend #+ map element hintEls
  pure legend

-- | Single tier hint item
tierHintItem :: (TierColor, Text, Text, Text) -> UI Element
tierHintItem (color, dieRange, tierName, desc) = do
  item <- UI.div #. "tier-hint-item"

  rangeEl <- UI.span #. "tier-hint-range"
    # set text (T.unpack dieRange)
    # set style [("color", T.unpack (tierColor color))]

  nameEl <- UI.span #. "tier-hint-name"
    # set text (T.unpack tierName)
    # set style [("color", T.unpack (tierColor color))]

  descEl <- UI.span #. "tier-hint-desc"
    # set text (T.unpack desc)

  void $ element item #+ [element rangeEl, element nameEl, element descEl]
  pure item

-- | Create a single die card with LLM-generated hint
dieCard
  :: GUIBridge state
  -> Position         -- ^ Current position
  -> (Int, (Int, Int, Text)) -- ^ (display number, (die value, index, hint))
  -> UI Element
dieCard bridge pos (displayNum, (dieValue, idx, hint)) = do
  let tier = calculateOutcome pos dieValue
      color = tierToColor tier

  card <- UI.div #. "dice-card"
    # set (attr "tabindex") "0"  -- Make focusable
    # set (attr "role") "option"
    # set (attr "aria-label") (T.unpack $ tierLabel tier <> " - " <> hint)
    # set style [("border-color", T.unpack (tierColor color))]

  -- Keyboard shortcut hint
  numHint <- UI.span #. "dice-num" # set text (show displayNum <> ".")

  faceEl <- dieFaceIcon dieValue

  tierEl <- UI.span #. "tier-label"
    # set text (T.unpack (tierLabel tier))
    # set style [("color", T.unpack (tierColor color))]

  -- LLM-generated hint showing what this specific outcome means
  hintEl <- UI.span #. "dice-hint"
    # set text (T.unpack hint)

  -- Click handler
  on UI.click card $ const $ liftIO $
    safeSubmitResponse bridge (ChoiceResponse idx)

  -- Keyboard handler (Enter or Space to select)
  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $  -- Enter or Space
      safeSubmitResponse bridge (ChoiceResponse idx)

  void $ element card #+ [element numHint, element faceEl, element tierEl, element hintEl]
  pure card

-- | Create a Font Awesome die face icon element
dieFaceIcon :: Int -> UI Element
dieFaceIcon n = do
  icon <- mkElement "i" #. ("die-face fa-solid " <> dieFaceClass n)
  pure icon

-- | Get the Font Awesome class for a die value
dieFaceClass :: Int -> String
dieFaceClass 1 = "fa-dice-one"
dieFaceClass 2 = "fa-dice-two"
dieFaceClass 3 = "fa-dice-three"
dieFaceClass 4 = "fa-dice-four"
dieFaceClass 5 = "fa-dice-five"
dieFaceClass 6 = "fa-dice-six"
dieFaceClass _ = "fa-question"

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
