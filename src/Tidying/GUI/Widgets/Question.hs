{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Question widgets for the Tidying GUI
--
-- Renders the Question DSL types as interactive widgets.
-- Supports ranked choice cards, confirmation dialogs, and text input.
--
-- = Design
--
-- * 'ProposeDisposition': First choice highlighted as "recommended", number keys 1-9
-- * 'Confirm': Yes/No buttons with Y/N keyboard shortcuts
-- * 'Choose': Option buttons with number keys 1-9
-- * 'FreeText': Text input with placeholder, Enter to submit
--
-- = Keyboard Support
--
-- All question types support keyboard navigation:
--
-- * Number keys (1-9) for quick selection in ProposeDisposition and Choose
-- * Y/N keys for Confirm questions
-- * Enter/Space on focused cards
-- * Tab to navigate between options
--
module Tidying.GUI.Widgets.Question
  ( -- * Main renderer
    renderQuestion
  , QuestionResult(..)
  ) where

import Control.Monad (void, when, forM)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core
  ( GUIBridge(..)
  , safeSubmitResponse
  , RequestResponse(..)
  )
import Tidying.Question
  ( Question(..)
  , Answer(..)
  , Choice(..)
  , ChoiceOption(..)
  , ItemDisposition(..)
  )

-- | Result of rendering a question
data QuestionResult = QuestionResult
  { qrElement :: Element
    -- ^ The rendered widget
  , qrFocusTarget :: Maybe Element
    -- ^ Element to auto-focus (if any)
  }

-- | Render a Question to interactive UI elements
--
-- The widget will call 'safeSubmitResponse' with a 'CustomResponse'
-- containing the JSON-encoded Answer when the user interacts.
renderQuestion :: GUIBridge state -> Question -> UI QuestionResult
renderQuestion bridge = \case
  ProposeDisposition item choices fallback ->
    renderDisposition bridge item choices fallback

  Confirm prompt defVal ->
    renderConfirm bridge prompt defVal

  Choose prompt qid options ->
    renderChoose bridge prompt qid options

  FreeText prompt placeholder ->
    renderFreeText bridge prompt placeholder

-- ══════════════════════════════════════════════════════════════
-- DISPOSITION (Ranked Choices)
-- ══════════════════════════════════════════════════════════════

-- | Render ProposeDisposition with ranked choices
--
-- First choice is highlighted as the agent's recommendation.
-- Includes optional fallback text input for "other" responses.
renderDisposition
  :: GUIBridge state
  -> Text           -- ^ Item description
  -> [Choice]       -- ^ Ranked choices (first = recommended)
  -> Maybe Text     -- ^ Fallback text placeholder
  -> UI QuestionResult
renderDisposition bridge item choices fallback = do
  -- Container needs tabindex for number key shortcuts
  container <- UI.div #. "question-disposition"
    # set (attr "tabindex") "-1"

  -- Item being decided
  itemEl <- UI.div #. "disposition-item"
    # set text (T.unpack $ "What about: " <> item)

  -- Choices container
  choicesEl <- UI.div #. "disposition-choices"
    # set (attr "role") "listbox"

  -- Render each choice, first one highlighted
  cards <- forM (zip [1..] choices) $ \(num, choice) ->
    mkDispositionCard bridge num (num == 1) choice

  void $ element choicesEl #+ map element cards

  -- Fallback text input (for "Other...")
  fallbackSection <- case fallback of
    Nothing -> UI.div # set style [("display", "none")]
    Just placeholder -> do
      section <- UI.div #. "disposition-fallback"

      otherBtn <- UI.button #. "disposition-other-btn"
        # set text "Other..."
        # set (attr "tabindex") "0"

      inputContainer <- UI.div #. "disposition-other-input"
        # set style [("display", "none")]

      inputEl <- UI.input #. "text-input"
        # set (attr "placeholder") (T.unpack placeholder)

      submitBtn <- UI.button #. "submit-btn" # set text "Submit"

      -- Show input when "Other..." clicked
      on UI.click otherBtn $ const $ do
        void $ element inputContainer # set style [("display", "flex")]
        void $ element otherBtn # set style [("display", "none")]
        UI.setFocus inputEl

      -- Submit custom location
      let submitOther = do
            val <- get value inputEl
            when (not (null val)) $ do
              let answer = DispositionAnswer (PlaceAt (T.pack val))
              liftIO $ safeSubmitResponse bridge (CustomResponse (toJSON answer))

      on UI.click submitBtn $ const submitOther
      on UI.keydown inputEl $ \code ->
        when (code == 13) submitOther  -- Enter

      void $ element inputContainer #+ [element inputEl, element submitBtn]
      void $ element section #+ [element otherBtn, element inputContainer]
      pure section

  -- Keyboard shortcuts 1-9 for quick selection
  on UI.keydown container $ \code -> do
    when (code >= 49 && code <= 57) $ do  -- Keys 1-9
      let num = code - 48
      when (num <= length choices) $ do
        let choice = choices !! (num - 1)
        liftIO $ safeSubmitResponse bridge
          (CustomResponse (toJSON (DispositionAnswer choice.choiceValue)))

  void $ element container #+ [element itemEl, element choicesEl, element fallbackSection]

  -- Focus container so number shortcuts work immediately
  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

-- | Create a disposition choice card
mkDispositionCard
  :: GUIBridge state
  -> Int         -- ^ Number (for keyboard shortcut)
  -> Bool        -- ^ Is this the recommended choice?
  -> Choice      -- ^ The choice
  -> UI Element
mkDispositionCard bridge num isRecommended choice = do
  let cardClass = if isRecommended
        then "disposition-card recommended"
        else "disposition-card"

  card <- UI.div #. cardClass
    # set (attr "tabindex") "0"
    # set (attr "role") "option"
    # set (attr "aria-label") (T.unpack $ dispositionLabel num isRecommended choice)

  -- Number hint
  numHint <- UI.span #. "choice-num" # set text (show num <> ". ")

  -- Label
  labelEl <- UI.span #. "disposition-label"
    # set text (T.unpack choice.choiceLabel)

  -- Recommended badge
  badge <- if isRecommended
    then UI.span #. "recommended-badge" # set text " (recommended)"
    else UI.span # set style [("display", "none")]

  -- Click handler
  on UI.click card $ const $ liftIO $ do
    let answer = DispositionAnswer choice.choiceValue
    safeSubmitResponse bridge (CustomResponse (toJSON answer))

  -- Keyboard handler
  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $ do  -- Enter or Space
      let answer = DispositionAnswer choice.choiceValue
      safeSubmitResponse bridge (CustomResponse (toJSON answer))

  void $ element card #+ [element numHint, element labelEl, element badge]
  pure card

dispositionLabel :: Int -> Bool -> Choice -> Text
dispositionLabel num isRec choice =
  "Option " <> T.pack (show num) <> ": " <> choice.choiceLabel
    <> if isRec then " (recommended)" else ""

-- ══════════════════════════════════════════════════════════════
-- CONFIRM (Yes/No)
-- ══════════════════════════════════════════════════════════════

-- | Render Confirm as Yes/No buttons
renderConfirm
  :: GUIBridge state
  -> Text    -- ^ Prompt
  -> Bool    -- ^ Default value (pre-selected)
  -> UI QuestionResult
renderConfirm bridge prompt defVal = do
  -- Container needs tabindex to receive keyboard events for Y/N shortcuts
  container <- UI.div #. "question-confirm"
    # set (attr "tabindex") "-1"

  promptEl <- UI.div #. "confirm-prompt"
    # set text (T.unpack prompt)

  buttonsRow <- UI.div #. "confirm-buttons"

  let yesClass = if defVal then "confirm-btn selected" else "confirm-btn"
      noClass = if not defVal then "confirm-btn selected" else "confirm-btn"

  yesBtn <- UI.button #. yesClass
    # set text "Yes"
    # set (attr "tabindex") "0"

  noBtn <- UI.button #. noClass
    # set text "No"
    # set (attr "tabindex") "0"

  on UI.click yesBtn $ const $ liftIO $ do
    safeSubmitResponse bridge (CustomResponse (toJSON (ConfirmAnswer True)))

  on UI.click noBtn $ const $ liftIO $ do
    safeSubmitResponse bridge (CustomResponse (toJSON (ConfirmAnswer False)))

  -- Keyboard shortcuts: Y for yes, N for no
  on UI.keydown container $ \code -> do
    when (code == 89) $ liftIO $  -- Y key
      safeSubmitResponse bridge (CustomResponse (toJSON (ConfirmAnswer True)))
    when (code == 78) $ liftIO $  -- N key
      safeSubmitResponse bridge (CustomResponse (toJSON (ConfirmAnswer False)))

  void $ element buttonsRow #+ [element yesBtn, element noBtn]
  void $ element container #+ [element promptEl, element buttonsRow]

  -- Focus container so Y/N shortcuts work immediately
  -- User can Tab to individual buttons for Enter/Space activation
  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

-- ══════════════════════════════════════════════════════════════
-- CHOOSE (Multi-Choice)
-- ══════════════════════════════════════════════════════════════

-- | Render Choose as option buttons
renderChoose
  :: GUIBridge state
  -> Text            -- ^ Prompt
  -> Text            -- ^ Question ID (for reveals)
  -> [ChoiceOption]  -- ^ Options
  -> UI QuestionResult
renderChoose bridge prompt _qid options = do
  -- Container needs tabindex for number key shortcuts
  container <- UI.div #. "question-choose"
    # set (attr "tabindex") "-1"

  promptEl <- UI.div #. "choose-prompt"
    # set text (T.unpack prompt)

  optionsEl <- UI.div #. "choose-options"
    # set (attr "role") "listbox"

  cards <- forM (zip [1..] options) $ \(num, opt) ->
    mkChooseCard bridge num opt

  void $ element optionsEl #+ map element cards

  -- Keyboard shortcuts 1-9
  on UI.keydown container $ \code -> do
    when (code >= 49 && code <= 57) $ do
      let num = code - 48
      when (num <= length options) $ do
        let opt = options !! (num - 1)
        liftIO $ safeSubmitResponse bridge
          (CustomResponse (toJSON (ChoiceAnswer opt.optionValue)))

  void $ element container #+ [element promptEl, element optionsEl]

  -- Focus container so number shortcuts (1-9) work immediately
  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

mkChooseCard :: GUIBridge state -> Int -> ChoiceOption -> UI Element
mkChooseCard bridge num opt = do
  card <- UI.div #. "choose-card"
    # set (attr "tabindex") "0"
    # set (attr "role") "option"

  numHint <- UI.span #. "choice-num" # set text (show num <> ". ")
  labelEl <- UI.span # set text (T.unpack opt.optionLabel)

  on UI.click card $ const $ liftIO $
    safeSubmitResponse bridge (CustomResponse (toJSON (ChoiceAnswer opt.optionValue)))

  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $
      safeSubmitResponse bridge (CustomResponse (toJSON (ChoiceAnswer opt.optionValue)))

  void $ element card #+ [element numHint, element labelEl]
  pure card

-- ══════════════════════════════════════════════════════════════
-- FREE TEXT
-- ══════════════════════════════════════════════════════════════

-- | Render FreeText as text input
renderFreeText
  :: GUIBridge state
  -> Text         -- ^ Prompt
  -> Maybe Text   -- ^ Placeholder
  -> UI QuestionResult
renderFreeText bridge prompt mPlaceholder = do
  container <- UI.div #. "question-freetext"

  promptEl <- UI.div #. "freetext-prompt"
    # set text (T.unpack prompt)

  inputRow <- UI.div #. "freetext-row"

  inputEl <- UI.input #. "text-input"
    # set (attr "placeholder") (maybe "Type here..." T.unpack mPlaceholder)

  submitBtn <- UI.button #. "submit-btn" # set text "Submit"

  let submit = do
        val <- get value inputEl
        when (not (null val)) $ liftIO $ do
          safeSubmitResponse bridge (CustomResponse (toJSON (TextAnswer (T.pack val))))

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit

  void $ element inputRow #+ [element inputEl, element submitBtn]
  void $ element container #+ [element promptEl, element inputRow]

  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just inputEl
    }
