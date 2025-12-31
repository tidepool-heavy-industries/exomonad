-- | Character creation UI widget
--
-- Multi-step flow for new game setup:
-- 1. Enter name
-- 2. Choose pronouns
-- 3. Choose archetype
-- 4. Enter background (freeform "who are you")
-- 5. View tarot spread
-- 6. Wait for LLM to generate scenario
module DM.GUI.Widgets.CharacterCreation
  ( characterCreationWidget
  , CharacterCreationResult(..)
  ) where

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (void, when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import DM.CharacterCreation
import DM.Tarot (TarotCard, drawSpread, cardName, cardMeaning)

-- | Result of character creation flow
data CharacterCreationResult
  = CharacterCreated CharacterChoices
  | CharacterCreationCancelled
  deriving (Show, Eq)

-- | Create the character creation widget
--
-- This is a self-contained flow that collects all inputs and returns
-- the completed CharacterChoices via an MVar.
characterCreationWidget :: MVar CharacterCreationResult -> UI Element
characterCreationWidget resultMVar = do
  container <- UI.div #. "character-creation"

  -- Title
  titleEl <- UI.h1 #. "creation-title" # set text "WELCOME TO DOSKVOL"
  subtitleEl <- UI.p #. "creation-subtitle"
    # set text "The eternal night awaits. Who are you?"

  -- Content area (will be replaced at each step)
  contentArea <- UI.div #. "creation-content"

  void $ element container #+ [element titleEl, element subtitleEl, element contentArea]

  -- Start with step 1: name input
  showNameStep contentArea resultMVar

  pure container

-- | Step 1: Enter name
showNameStep :: Element -> MVar CharacterCreationResult -> UI ()
showNameStep contentArea resultMVar = do
  void $ element contentArea # set children []

  stepEl <- UI.div #. "creation-step"

  promptEl <- UI.div #. "creation-prompt"
    # set text "What name do you go by in the shadows?"

  inputEl <- UI.input #. "creation-input"
    # set (attr "placeholder") "Enter your name..."
    # set (attr "autocomplete") "off"

  nextBtn <- UI.button #. "creation-btn" # set text "Continue"
    # set (attr "disabled") "disabled"

  -- Enable button when name has content
  on UI.keyup inputEl $ \_ -> do
    val <- get value inputEl
    if null val
      then void $ element nextBtn # set (attr "disabled") "disabled"
      else void $ element nextBtn # set (attr "disabled") ""

  -- Handle submit
  let submit = do
        val <- get value inputEl
        unless (null val) $ do
          let name = T.pack val
          showPronounsStep contentArea resultMVar name

  on UI.click nextBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element stepEl #+ [element promptEl, element inputEl, element nextBtn]
  void $ element contentArea #+ [element stepEl]

  -- Focus the input
  runFunction $ ffi "$(%1).focus()" inputEl

-- | Step 2: Choose pronouns
showPronounsStep :: Element -> MVar CharacterCreationResult -> Text -> UI ()
showPronounsStep contentArea resultMVar name = do
  void $ element contentArea # set children []

  stepEl <- UI.div #. "creation-step"

  promptEl <- UI.div #. "creation-prompt"
    # set text "How should others refer to you?"

  choicesEl <- UI.div #. "creation-choices"

  let pronounOptions =
        [ (HeHim, "He/Him", "Traditional masculine pronouns")
        , (SheHer, "She/Her", "Traditional feminine pronouns")
        , (TheyThem, "They/Them", "Gender-neutral pronouns")
        ]

  mapM_ (mkPronounCard choicesEl contentArea resultMVar name) pronounOptions

  -- Custom option
  customEl <- UI.div #. "creation-custom"
  customLabel <- UI.span # set text "Or enter custom: "
  customInput <- UI.input #. "creation-input-small"
    # set (attr "placeholder") "e.g., xe/xem"
  customBtn <- UI.button #. "creation-btn-small" # set text "Use"

  on UI.click customBtn $ const $ do
    val <- get value customInput
    unless (null val) $ do
      let pronouns = Custom (T.pack val)
      showArchetypeStep contentArea resultMVar name pronouns

  void $ element customEl #+ [element customLabel, element customInput, element customBtn]

  void $ element stepEl #+ [element promptEl, element choicesEl, element customEl]
  void $ element contentArea #+ [element stepEl]

mkPronounCard :: Element -> Element -> MVar CharacterCreationResult -> Text -> (Pronouns, String, String) -> UI ()
mkPronounCard container contentArea resultMVar name (pronouns, label, desc) = do
  card <- UI.div #. "creation-card"
    # set (attr "tabindex") "0"

  labelEl <- UI.div #. "creation-card-label" # set text label
  descEl <- UI.div #. "creation-card-desc" # set text desc

  on UI.click card $ const $
    showArchetypeStep contentArea resultMVar name pronouns

  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $  -- Enter or Space
      showArchetypeStep contentArea resultMVar name pronouns

  void $ element card #+ [element labelEl, element descEl]
  void $ element container #+ [element card]

-- | Step 3: Choose archetype
showArchetypeStep :: Element -> MVar CharacterCreationResult -> Text -> Pronouns -> UI ()
showArchetypeStep contentArea resultMVar name pronouns = do
  void $ element contentArea # set children []

  stepEl <- UI.div #. "creation-step"

  promptEl <- UI.div #. "creation-prompt"
    # set text "What kind of scoundrel are you?"

  choicesEl <- UI.div #. "creation-choices archetype-grid"

  mapM_ (mkArchetypeCard choicesEl contentArea resultMVar name pronouns) allArchetypes

  void $ element stepEl #+ [element promptEl, element choicesEl]
  void $ element contentArea #+ [element stepEl]

mkArchetypeCard :: Element -> Element -> MVar CharacterCreationResult -> Text -> Pronouns -> Archetype -> UI ()
mkArchetypeCard container contentArea resultMVar name pronouns archetype = do
  card <- UI.div #. "creation-card archetype-card"
    # set (attr "tabindex") "0"

  labelEl <- UI.div #. "creation-card-label" # set text (T.unpack $ archetypeName archetype)
  descEl <- UI.div #. "creation-card-desc" # set text (T.unpack $ archetypeDescription archetype)

  on UI.click card $ const $
    showBackgroundStep contentArea resultMVar name pronouns archetype

  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $
      showBackgroundStep contentArea resultMVar name pronouns archetype

  void $ element card #+ [element labelEl, element descEl]
  void $ element container #+ [element card]

-- | Step 4: Enter background
showBackgroundStep :: Element -> MVar CharacterCreationResult -> Text -> Pronouns -> Archetype -> UI ()
showBackgroundStep contentArea resultMVar name pronouns archetype = do
  void $ element contentArea # set children []

  stepEl <- UI.div #. "creation-step"

  promptEl <- UI.div #. "creation-prompt"
    # set text "Who are you? A few sentences about your past, your situation, what drives you."

  -- Use same style as name input, no placeholder hint
  inputEl <- UI.input #. "creation-input"
    # set (attr "autocomplete") "off"

  nextBtn <- UI.button #. "creation-btn" # set text "Continue"
    # set (attr "disabled") "disabled"

  -- Enable button when background has any content (same as name step)
  on UI.keyup inputEl $ \_ -> do
    val <- get value inputEl
    if null val
      then void $ element nextBtn # set (attr "disabled") "disabled"
      else void $ element nextBtn # set (attr "disabled") ""

  -- Handle submit
  let submit = do
        val <- get value inputEl
        unless (null val) $ do
          let background = T.pack val
          showTarotStep contentArea resultMVar name pronouns archetype background

  on UI.click nextBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element stepEl #+ [element promptEl, element inputEl, element nextBtn]
  void $ element contentArea #+ [element stepEl]

  -- Focus the input
  runFunction $ ffi "$(%1).focus()" inputEl

-- | Step 5: Tarot spread reveal
showTarotStep :: Element -> MVar CharacterCreationResult -> Text -> Pronouns -> Archetype -> Text -> UI ()
showTarotStep contentArea resultMVar name pronouns archetype background = do
  void $ element contentArea # set children []

  stepEl <- UI.div #. "creation-step tarot-step"

  promptEl <- UI.div #. "creation-prompt"
    # set text "The cards reveal your fate..."

  -- Draw the spread
  cards <- liftIO $ drawSpread 3

  cardsEl <- UI.div #. "tarot-spread"

  case cards of
    [past, present, future] -> do
      pastCard <- mkTarotCard "PAST" past
      presentCard <- mkTarotCard "PRESENT" present
      futureCard <- mkTarotCard "FUTURE" future
      void $ element cardsEl #+ [element pastCard, element presentCard, element futureCard]
    _ -> pure ()

  continueBtn <- UI.button #. "creation-btn tarot-continue"
    # set text "Begin Your Story"

  on UI.click continueBtn $ const $ do
    let choices = CharacterChoices name pronouns archetype background cards
    liftIO $ putMVar resultMVar (CharacterCreated choices)

  void $ element stepEl #+ [element promptEl, element cardsEl, element continueBtn]
  void $ element contentArea #+ [element stepEl]

mkTarotCard :: String -> TarotCard -> UI Element
mkTarotCard position card = do
  cardEl <- UI.div #. "tarot-card"

  posEl <- UI.div #. "tarot-position" # set text position
  nameEl <- UI.div #. "tarot-name" # set text (T.unpack $ cardName card)
  meaningEl <- UI.div #. "tarot-meaning" # set text (T.unpack $ cardMeaning card)

  void $ element cardEl #+ [element posEl, element nameEl, element meaningEl]
  pure cardEl
