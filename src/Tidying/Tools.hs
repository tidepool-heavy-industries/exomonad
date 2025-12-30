{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Tidying Tools (mid-turn LLM capabilities)
--
-- The key tool is 'ProposeDisposition' which lets the LLM ask the user
-- about where an item should go. The LLM generates the question based on
-- what it sees (photo analysis), then waits for user confirmation.
--
-- = Flow
--
-- 1. LLM analyzes photo, sees "blue mug on papers"
-- 2. LLM calls propose_disposition with its best guesses
-- 3. Tool presents choices to user via QuestionHandler (IO callback)
-- 4. User taps button or types correction
-- 5. Answer returns to LLM as tool result
-- 6. LLM continues: "Kitchen counter. Now those cables..."
--
-- = Effect Architecture
--
-- Tools use IO callbacks (QuestionHandler) rather than the QuestionUI effect
-- because tools run inside the LLM interpreter, after QuestionUI has been
-- interpreted. The QuestionHandler is passed to the dispatcher at setup time.
--
module Tidying.Tools
  ( -- * Tools
    ProposeDisposition(..)
  , AskSpaceFunction(..)
  , ConfirmDone(..)

    -- * Tool Inputs/Outputs
  , ProposeDispositionInput(..)
  , ProposeDispositionResult(..)
  , ProposedChoice(..)
  , AskSpaceFunctionInput(..)
  , AskSpaceFunctionResult(..)
  , ConfirmDoneInput(..)
  , ConfirmDoneResult(..)

    -- * Tool Registration
  , tidyingTools
  , makeTidyingDispatcher
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON(..), FromJSON(..), Result(..), object, (.=), (.:), (.:?), (.!=), withObject, fromJSON)
import GHC.Generics (Generic)
import Effectful (Eff, (:>), IOE)

import Tidepool.Effect
  ( State, Emit, Log
  , emit, logWarn, modify
  , QuestionHandler
  , ToolResult(..)
  )
import Tidepool.Schema (objectSchema, arraySchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Tidying.Question as Q
import Tidying.State (SessionState(..), PhaseData(..))
import Tidying.Events (TidyingEvent(..))
import Tidying.Types (SpaceFunction(..))

-- ══════════════════════════════════════════════════════════════
-- PROPOSE DISPOSITION TOOL
-- ══════════════════════════════════════════════════════════════

-- | Tool for proposing item disposition
-- LLM generates choices based on photo analysis, user confirms
data ProposeDisposition = ProposeDisposition
  deriving (Show, Eq, Generic)

-- | A proposed choice from the LLM
data ProposedChoice = ProposedChoice
  { pcLabel :: Text           -- ^ "Kitchen counter"
  , pcDisposition :: Text     -- ^ "kitchen" or "trash" or "donate" etc
  , pcIsTrash :: Bool         -- ^ True if this is a trash/dispose option
  , pcIsDonate :: Bool        -- ^ True if this is a donate option
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProposedChoice where
  toJSON ProposedChoice{..} = object
    [ "label" .= pcLabel
    , "disposition" .= pcDisposition
    , "is_trash" .= pcIsTrash
    , "is_donate" .= pcIsDonate
    ]

instance FromJSON ProposedChoice where
  parseJSON = withObject "ProposedChoice" $ \o -> ProposedChoice
    <$> o .: "label"
    <*> o .: "disposition"
    <*> o .:? "is_trash" .!= False
    <*> o .:? "is_donate" .!= False

-- | Input from LLM
data ProposeDispositionInput = ProposeDispositionInput
  { pdiItem :: Text              -- ^ "the blue mug on the stack of papers"
  , pdiChoices :: [ProposedChoice]  -- ^ Ranked choices, first = best guess
  , pdiFallback :: Maybe Text    -- ^ Placeholder for text input if all wrong
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result returned to LLM
data ProposeDispositionResult = ProposeDispositionResult
  { pdrDisposition :: Q.ItemDisposition  -- ^ Where the item goes
  , pdrWasCorrection :: Bool              -- ^ True if user typed custom location
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Schema for LLM
proposeDispositionSchema :: Value
proposeDispositionSchema = schemaToValue $ objectSchema
  [ ("item", describeField "item" "Specific description of the item (e.g., 'the blue mug on the papers')" (emptySchema TString))
  , ("choices", describeField "choices" "2-4 ranked choices. First is your best guess. Each has label (shown to user), disposition (location or 'trash'/'donate'), is_trash, is_donate flags."
      (arraySchema $ objectSchema
        [ ("label", emptySchema TString)
        , ("disposition", emptySchema TString)
        , ("is_trash", emptySchema TBoolean)
        , ("is_donate", emptySchema TBoolean)
        ]
        ["label", "disposition"]))
  , ("fallback", describeField "fallback" "Optional placeholder for text input if all choices are wrong" (emptySchema TString))
  ]
  ["item", "choices"]

-- | Execute the tool
--
-- Takes a QuestionHandler because tools run inside the LLM interpreter
-- where QuestionUI effect has already been consumed.
--
-- Wraps the question handler in try/catch so GUI crashes don't crash the agent.
executeProposeDisposition
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler
  -> ProposeDispositionInput
  -> Eff es ProposeDispositionResult
executeProposeDisposition askQuestion input = do
  -- Emit that we're proposing
  emit $ ItemProposed input.pdiItem (map (.pcLabel) input.pdiChoices)

  -- Convert LLM choices to Question DSL
  let choices = map toChoice input.pdiChoices
      question = Q.ProposeDisposition
        { Q.pdItem = input.pdiItem
        , Q.pdChoices = choices
        , Q.pdFallback = input.pdiFallback
        }

  -- Ask user via IO callback (wrapped in try/catch for robustness)
  result <- liftIO $ try @SomeException $ askQuestion question

  case result of
    Left err -> do
      -- Question handler failed (GUI crash, MVar deadlock, etc)
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure ProposeDispositionResult
        { pdrDisposition = Q.SkipForNow
        , pdrWasCorrection = False
        }

    Right answer ->
      -- Process answer
      case answer of
        Q.DispositionAnswer disp -> do
          emit $ UserConfirmed input.pdiItem (dispositionToText disp)
          pure ProposeDispositionResult
            { pdrDisposition = disp
            , pdrWasCorrection = False
            }

        Q.TextAnswer loc -> do
          emit $ UserCorrected input.pdiItem loc
          pure ProposeDispositionResult
            { pdrDisposition = Q.PlaceAt loc
            , pdrWasCorrection = True
            }

        _ -> do
          -- Fallback - treat as skip
          pure ProposeDispositionResult
            { pdrDisposition = Q.SkipForNow
            , pdrWasCorrection = False
            }

  where
    toChoice :: ProposedChoice -> Q.Choice
    toChoice pc = Q.Choice
      { Q.choiceLabel = pc.pcLabel
      , Q.choiceValue = parseDisposition pc
      }

    parseDisposition :: ProposedChoice -> Q.ItemDisposition
    parseDisposition pc
      | pc.pcIsTrash = Q.Trash
      | pc.pcIsDonate = Q.Donate
      | pc.pcDisposition == "trash" = Q.Trash
      | pc.pcDisposition == "donate" = Q.Donate
      | pc.pcDisposition == "recycle" = Q.Recycle
      | pc.pcDisposition == "skip" = Q.SkipForNow
      | otherwise = Q.PlaceAt pc.pcDisposition

    dispositionToText :: Q.ItemDisposition -> Text
    dispositionToText Q.Trash = "trash"
    dispositionToText Q.Donate = "donate"
    dispositionToText Q.Recycle = "recycle"
    dispositionToText Q.SkipForNow = "skip"
    dispositionToText Q.NeedMoreInfo = "need_more_info"
    dispositionToText (Q.PlaceAt loc) = loc

-- ══════════════════════════════════════════════════════════════
-- ASK SPACE FUNCTION TOOL
-- ══════════════════════════════════════════════════════════════

-- | Tool for asking what the space needs to DO
data AskSpaceFunction = AskSpaceFunction
  deriving (Show, Eq, Generic)

data AskSpaceFunctionInput = AskSpaceFunctionInput
  { asfiContext :: Maybe Text  -- ^ Optional context about the space
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AskSpaceFunctionResult = AskSpaceFunctionResult
  { asfrFunction :: Text  -- ^ "workspace", "creative", "bedroom", etc.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

askSpaceFunctionSchema :: Value
askSpaceFunctionSchema = schemaToValue $ objectSchema
  [ ("context", describeField "context" "Optional context about what you observe in the space" (emptySchema TString))
  ]
  []

executeAskSpaceFunction
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler
  -> AskSpaceFunctionInput
  -> Eff es AskSpaceFunctionResult
executeAskSpaceFunction askQuestion _input = do
  -- Use the smart constructor from Question.hs (wrapped in try/catch)
  result <- liftIO $ try @SomeException $ askQuestion askFunction

  case result of
    Left err -> do
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure AskSpaceFunctionResult { asfrFunction = "living" }

    Right answer ->
      case answer of
        Q.ChoiceAnswer value -> do
          emit $ FunctionChosen value
          -- Sync to state so subsequent turns see the function choice
          modify @SessionState $ \st -> updateFunctionInState st (SpaceFunction value)
          pure AskSpaceFunctionResult { asfrFunction = value }

        Q.TextAnswer custom -> do
          emit $ FunctionChosen custom
          -- Sync to state so subsequent turns see the function choice
          modify @SessionState $ \st -> updateFunctionInState st (SpaceFunction custom)
          pure AskSpaceFunctionResult { asfrFunction = custom }

        _ ->
          -- Fallback
          pure AskSpaceFunctionResult { asfrFunction = "living" }

-- | Helper to update function in PhaseData (only valid in Surveying)
updateFunctionInState :: SessionState -> SpaceFunction -> SessionState
updateFunctionInState st fn = case st.phaseData of
  SurveyingData _ anchors ->
    st { phaseData = SurveyingData (Just fn) anchors }
  -- In other phases, function is already set and can't be changed
  _ -> st

-- ══════════════════════════════════════════════════════════════
-- CONFIRM DONE TOOL
-- ══════════════════════════════════════════════════════════════

-- | Tool for confirming user wants to stop
data ConfirmDone = ConfirmDone
  deriving (Show, Eq, Generic)

data ConfirmDoneInput = ConfirmDoneInput
  { cdiItemsProcessed :: Int   -- ^ How many items handled so far
  , cdiMessage :: Maybe Text   -- ^ Optional message to show
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConfirmDoneResult = ConfirmDoneResult
  { cdrConfirmed :: Bool  -- ^ True if user wants to stop
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

confirmDoneSchema :: Value
confirmDoneSchema = schemaToValue $ objectSchema
  [ ("items_processed", describeField "items_processed" "Number of items handled this session" (emptySchema TNumber))
  , ("message", describeField "message" "Optional message to show (e.g., 'Great progress!')" (emptySchema TString))
  ]
  ["items_processed"]

executeConfirmDone
  :: ( Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler
  -> ConfirmDoneInput
  -> Eff es ConfirmDoneResult
executeConfirmDone askQuestion input = do
  let prompt = maybe
        ("Done for now? (" <> T.pack (show input.cdiItemsProcessed) <> " items handled)")
        id
        input.cdiMessage

  result <- liftIO $ try @SomeException $ askQuestion $ Q.Confirm
    { Q.cfPrompt = prompt
    , Q.cfDefault = True  -- Default to "yes, done"
    }

  case result of
    Left err -> do
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure ConfirmDoneResult { cdrConfirmed = False }

    Right answer ->
      case answer of
        Q.ConfirmAnswer True -> do
          emit SessionConfirmedDone
          pure ConfirmDoneResult { cdrConfirmed = True }

        _ ->
          pure ConfirmDoneResult { cdrConfirmed = False }

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All tidying tools as JSON for API
tidyingTools :: [Value]
tidyingTools =
  [ object
      [ "name" .= ("propose_disposition" :: Text)
      , "description" .= ("Propose where an item should go. Present 2-4 choices ranked by likelihood (first = best guess). User taps to confirm or types correction. Use this for EVERY item you see - don't just describe, propose!" :: Text)
      , "input_schema" .= proposeDispositionSchema
      ]
  , object
      [ "name" .= ("ask_space_function" :: Text)
      , "description" .= ("Ask what the space needs to DO. Use early in session to understand user's goal. Options: workspace, creative, bedroom, storage, living." :: Text)
      , "input_schema" .= askSpaceFunctionSchema
      ]
  , object
      [ "name" .= ("confirm_done" :: Text)
      , "description" .= ("Confirm user wants to end session. Shows items processed count." :: Text)
      , "input_schema" .= confirmDoneSchema
      ]
  ]

-- | Create dispatcher for tidying tools
--
-- Takes a QuestionHandler to route questions to the GUI.
-- Tools need IOE for the question handler callbacks.
-- Tools use Log to warn about failures (try/catch on question handler).
makeTidyingDispatcher
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler  -- ^ Handler for routing questions to user
  -> Text             -- ^ Tool name
  -> Value            -- ^ Tool input
  -> Eff es (Either Text ToolResult)
makeTidyingDispatcher handler "propose_disposition" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse propose_disposition input: " <> T.pack err
    Success parsed -> do
      result <- executeProposeDisposition handler parsed
      pure $ Right $ ToolSuccess $ toJSON result

makeTidyingDispatcher handler "ask_space_function" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse ask_space_function input: " <> T.pack err
    Success parsed -> do
      result <- executeAskSpaceFunction handler parsed
      pure $ Right $ ToolSuccess $ toJSON result

makeTidyingDispatcher handler "confirm_done" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse confirm_done input: " <> T.pack err
    Success parsed -> do
      result <- executeConfirmDone handler parsed
      pure $ Right $ ToolSuccess $ toJSON result

makeTidyingDispatcher _ name _ =
  pure $ Left $ "Unknown tidying tool: " <> name
