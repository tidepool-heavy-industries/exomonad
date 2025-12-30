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
import Data.IORef
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

-- | Wrap answer with path context if path is present
wrapWithPath :: Maybe [(Text, Text)] -> Answer -> Answer
wrapWithPath Nothing answer = answer
wrapWithPath (Just path) answer = AnswerWithPath answer path

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
renderDisposition bridge item choices fallback =
  renderDispositionWithPath bridge item choices fallback Nothing

-- | Render disposition with optional path context for terminal reveals
renderDispositionWithPath
  :: GUIBridge state
  -> Text           -- ^ Item description
  -> [Choice]       -- ^ Ranked choices (first = recommended)
  -> Maybe Text     -- ^ Fallback text placeholder
  -> Maybe [(Text, Text)]  -- ^ Accumulated path from parent Choose (for AnswerWithPath)
  -> UI QuestionResult
renderDispositionWithPath bridge item choices fallback mPath = do
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
    mkDispositionCard bridge mPath num (num == 1) choice

  void $ element choicesEl #+ map element cards

  -- "Other" option - always shown so user can override agent's suggestions
  fallbackSection <- do
    let placeholder = maybe "Where should it go?" id fallback

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
            let baseAnswer = DispositionAnswer (PlaceAt (T.pack val))
                answer = wrapWithPath mPath baseAnswer
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
            baseAnswer = DispositionAnswer choice.choiceValue
            answer = wrapWithPath mPath baseAnswer
        liftIO $ safeSubmitResponse bridge (CustomResponse (toJSON answer))

  void $ element container #+ [element itemEl, element choicesEl, element fallbackSection]

  -- Focus container so number shortcuts work immediately
  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

-- | Create a disposition choice card
mkDispositionCard
  :: GUIBridge state
  -> Maybe [(Text, Text)]  -- ^ Path context for AnswerWithPath
  -> Int         -- ^ Number (for keyboard shortcut)
  -> Bool        -- ^ Is this the recommended choice?
  -> Choice      -- ^ The choice
  -> UI Element
mkDispositionCard bridge mPath num isRecommended choice = do
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
    let baseAnswer = DispositionAnswer choice.choiceValue
        answer = wrapWithPath mPath baseAnswer
    safeSubmitResponse bridge (CustomResponse (toJSON answer))

  -- Keyboard handler
  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) $ liftIO $ do  -- Enter or Space
      let baseAnswer = DispositionAnswer choice.choiceValue
          answer = wrapWithPath mPath baseAnswer
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
renderConfirm bridge prompt defVal =
  renderConfirmWithPath bridge prompt defVal Nothing

-- | Render Confirm with optional path context for terminal reveals
renderConfirmWithPath
  :: GUIBridge state
  -> Text    -- ^ Prompt
  -> Bool    -- ^ Default value (pre-selected)
  -> Maybe [(Text, Text)]  -- ^ Accumulated path from parent Choose
  -> UI QuestionResult
renderConfirmWithPath bridge prompt defVal mPath = do
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
    let answer = wrapWithPath mPath (ConfirmAnswer True)
    safeSubmitResponse bridge (CustomResponse (toJSON answer))

  on UI.click noBtn $ const $ liftIO $ do
    let answer = wrapWithPath mPath (ConfirmAnswer False)
    safeSubmitResponse bridge (CustomResponse (toJSON answer))

  -- Keyboard shortcuts: Y for yes, N for no
  on UI.keydown container $ \code -> do
    when (code == 89) $ liftIO $ do  -- Y key
      let answer = wrapWithPath mPath (ConfirmAnswer True)
      safeSubmitResponse bridge (CustomResponse (toJSON answer))
    when (code == 78) $ liftIO $ do  -- N key
      let answer = wrapWithPath mPath (ConfirmAnswer False)
      safeSubmitResponse bridge (CustomResponse (toJSON answer))

  void $ element buttonsRow #+ [element yesBtn, element noBtn]
  void $ element container #+ [element promptEl, element buttonsRow]

  -- Focus container so Y/N shortcuts work immediately
  -- User can Tab to individual buttons for Enter/Space activation
  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

-- ══════════════════════════════════════════════════════════════
-- CHOOSE (Multi-Choice with Reveals)
-- ══════════════════════════════════════════════════════════════

-- | Render Choose as option buttons, with support for branching reveals
--
-- When an option has reveals, clicking it shows the follow-up questions
-- instead of submitting immediately. Answers accumulate until reaching
-- a leaf option (no reveals), then all answers are submitted together.
renderChoose
  :: GUIBridge state
  -> Text            -- ^ Prompt
  -> Text            -- ^ Question ID
  -> [ChoiceOption]  -- ^ Options
  -> UI QuestionResult
renderChoose bridge prompt qid options = do
  -- Accumulator for answers as user walks the tree
  answersRef <- liftIO $ newIORef ([] :: [(Text, Text)])

  -- Container needs tabindex for number key shortcuts
  container <- UI.div #. "question-choose"
    # set (attr "tabindex") "-1"

  -- Render this level
  renderChooseLevel bridge answersRef container qid prompt options

  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just container
    }

-- | Render one level of a Choose question tree
renderChooseLevel
  :: GUIBridge state
  -> IORef [(Text, Text)]  -- ^ Accumulated answers
  -> Element               -- ^ Container to render into
  -> Text                  -- ^ Question ID for this level
  -> Text                  -- ^ Prompt
  -> [ChoiceOption]        -- ^ Options
  -> UI ()
renderChooseLevel bridge answersRef container qid prompt options = do
  -- Clear container
  void $ element container # set children []

  promptEl <- UI.div #. "choose-prompt"
    # set text (T.unpack prompt)

  optionsEl <- UI.div #. "choose-options"
    # set (attr "role") "listbox"

  -- Container for revealed follow-up questions
  revealsContainer <- UI.div #. "reveals-container"

  cards <- forM (zip [1..] options) $ \(num, opt) ->
    mkChooseCardWithReveals bridge answersRef container revealsContainer qid num opt

  void $ element optionsEl #+ map element cards

  -- "Other" option for custom text input
  otherSection <- mkOtherSection bridge answersRef qid

  -- Keyboard shortcuts 1-9
  on UI.keydown container $ \code -> do
    when (code >= 49 && code <= 57) $ do
      let num = code - 48
      when (num <= length options) $ do
        let opt = options !! (num - 1)
        handleOptionSelected bridge answersRef container revealsContainer qid opt

  void $ element container #+ [element promptEl, element optionsEl, element revealsContainer, element otherSection]

-- | Create a choice card that handles reveals
mkChooseCardWithReveals
  :: GUIBridge state
  -> IORef [(Text, Text)]  -- ^ Answer accumulator
  -> Element               -- ^ Parent container (for re-rendering)
  -> Element               -- ^ Reveals container
  -> Text                  -- ^ Question ID
  -> Int                   -- ^ Option number
  -> ChoiceOption          -- ^ The option
  -> UI Element
mkChooseCardWithReveals bridge answersRef _parentContainer revealsContainer qid num opt = do
  let hasReveals = not (null opt.optionReveals)

  card <- UI.div #. ("choose-card" <> if hasReveals then " has-reveals" else "")
    # set (attr "tabindex") "0"
    # set (attr "role") "option"

  numHint <- UI.span #. "choice-num" # set text (show num <> ". ")
  labelEl <- UI.span # set text (T.unpack opt.optionLabel)

  -- Indicator for options with follow-ups (arrow shows there's more)
  mIndicator <- if hasReveals
    then Just <$> (UI.span #. "reveals-indicator" # set text " →")
    else pure Nothing

  let handleClick = handleOptionSelected bridge answersRef _parentContainer revealsContainer qid opt

  on UI.click card $ const handleClick

  on UI.keydown card $ \code ->
    when (code == 13 || code == 32) handleClick

  case mIndicator of
    Just indicator -> void $ element card #+ [element numHint, element labelEl, element indicator]
    Nothing -> void $ element card #+ [element numHint, element labelEl]

  pure card

-- | Handle when an option is selected
handleOptionSelected
  :: GUIBridge state
  -> IORef [(Text, Text)]
  -> Element  -- ^ Parent container
  -> Element  -- ^ Reveals container
  -> Text     -- ^ Question ID
  -> ChoiceOption
  -> UI ()
handleOptionSelected bridge answersRef _parentContainer revealsContainer qid opt = do
  -- Record this answer
  liftIO $ modifyIORef' answersRef ((qid, opt.optionValue) :)

  case opt.optionReveals of
    [] -> do
      -- Leaf node - submit all accumulated answers
      answers <- liftIO $ readIORef answersRef
      liftIO $ safeSubmitResponse bridge
        (CustomResponse (toJSON (AnswerPath (reverse answers))))

    (firstReveal:_) -> do
      -- Has reveals - render the first one in the reveals container
      void $ element revealsContainer # set children []
      -- Get accumulated path for terminal reveals
      currentPath <- liftIO $ reverse <$> readIORef answersRef
      case firstReveal of
        Choose revPrompt revQid revOptions ->
          -- Choose chains continue accumulating answers via IORef
          renderChooseLevel bridge answersRef revealsContainer revQid revPrompt revOptions

        ProposeDisposition item choices fallback -> do
          -- Terminal reveal - disposition widget wraps answer with path
          result <- renderDispositionWithPath bridge item choices fallback (Just currentPath)
          void $ element revealsContainer #+ [element result.qrElement]
          maybe (pure ()) UI.setFocus result.qrFocusTarget

        FreeText prompt placeholder -> do
          -- Terminal reveal - text input wraps answer with path
          result <- renderFreeTextWithPath bridge prompt placeholder (Just currentPath)
          void $ element revealsContainer #+ [element result.qrElement]
          maybe (pure ()) UI.setFocus result.qrFocusTarget

        Confirm prompt defVal -> do
          -- Terminal reveal - confirm widget wraps answer with path
          result <- renderConfirmWithPath bridge prompt defVal (Just currentPath)
          void $ element revealsContainer #+ [element result.qrElement]
          maybe (pure ()) UI.setFocus result.qrFocusTarget

-- | Create the "Other..." section for free text input
mkOtherSection :: GUIBridge state -> IORef [(Text, Text)] -> Text -> UI Element
mkOtherSection bridge answersRef qid = do
  section <- UI.div #. "choose-other"

  otherBtn <- UI.button #. "disposition-other-btn"
    # set text "Other..."
    # set (attr "tabindex") "0"

  inputContainer <- UI.div #. "disposition-other-input"
    # set style [("display", "none")]

  inputEl <- UI.input #. "text-input"
    # set (attr "placeholder") "Type your answer..."

  submitBtn <- UI.button #. "submit-btn" # set text "Submit"

  on UI.click otherBtn $ const $ do
    void $ element inputContainer # set style [("display", "flex")]
    void $ element otherBtn # set style [("display", "none")]
    UI.setFocus inputEl

  let submitOther = do
        val <- get value inputEl
        when (not (null val)) $ do
          -- Record custom answer and submit
          liftIO $ modifyIORef' answersRef ((qid, T.pack val) :)
          answers <- liftIO $ readIORef answersRef
          liftIO $ safeSubmitResponse bridge
            (CustomResponse (toJSON (AnswerPath (reverse answers))))

  on UI.click submitBtn $ const submitOther
  on UI.keydown inputEl $ \code ->
    when (code == 13) submitOther

  void $ element inputContainer #+ [element inputEl, element submitBtn]
  void $ element section #+ [element otherBtn, element inputContainer]
  pure section

-- ══════════════════════════════════════════════════════════════
-- FREE TEXT
-- ══════════════════════════════════════════════════════════════

-- | Render FreeText as text input
renderFreeText
  :: GUIBridge state
  -> Text         -- ^ Prompt
  -> Maybe Text   -- ^ Placeholder
  -> UI QuestionResult
renderFreeText bridge prompt mPlaceholder =
  renderFreeTextWithPath bridge prompt mPlaceholder Nothing

-- | Render FreeText with optional path context for terminal reveals
renderFreeTextWithPath
  :: GUIBridge state
  -> Text         -- ^ Prompt
  -> Maybe Text   -- ^ Placeholder
  -> Maybe [(Text, Text)]  -- ^ Accumulated path from parent Choose
  -> UI QuestionResult
renderFreeTextWithPath bridge prompt mPlaceholder mPath = do
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
          let answer = wrapWithPath mPath (TextAnswer (T.pack val))
          safeSubmitResponse bridge (CustomResponse (toJSON answer))

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit

  void $ element inputRow #+ [element inputEl, element submitBtn]
  void $ element container #+ [element promptEl, element inputRow]

  pure QuestionResult
    { qrElement = container
    , qrFocusTarget = Just inputEl
    }
