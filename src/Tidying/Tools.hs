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
-- 3. Tool presents choices to user via Question DSL
-- 4. User taps button or types correction
-- 5. Answer returns to LLM as tool result
-- 6. LLM continues: "Kitchen counter. Now those cables..."
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

    -- * Events
  , TidyingToolEvent(..)

    -- * Tool Registration
  , tidyingTools
  , makeTidyingDispatcher
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON(..), FromJSON(..), Result(..), object, (.=), (.:), (.:?), (.!=), withObject, fromJSON)
import GHC.Generics (Generic)
import Effectful (Eff, (:>))

import Tidepool.Effect
  ( State, Emit, RequestInput, Random, QuestionUI
  , emit, requestQuestion
  )
import Tidepool.Schema (objectSchema, arraySchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Tidying.Question as Q
import Tidying.State (SessionState)

-- ══════════════════════════════════════════════════════════════
-- EVENTS
-- ══════════════════════════════════════════════════════════════

-- | Events emitted by tidying tools
data TidyingToolEvent
  = ItemProposed Text [Text]      -- ^ Item and proposed dispositions
  | UserConfirmed Text Text       -- ^ Item and chosen disposition
  | UserCorrected Text Text       -- ^ Item and user-provided location
  | FunctionChosen Text           -- ^ Space function selected
  | SessionConfirmedDone          -- ^ User confirmed session is done
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
executeProposeDisposition
  :: ( State SessionState :> es
     , Emit TidyingToolEvent :> es
     , QuestionUI :> es
     )
  => ProposeDispositionInput
  -> Eff es ProposeDispositionResult
executeProposeDisposition input = do
  -- Emit that we're proposing
  emit $ ItemProposed input.pdiItem (map (.pcLabel) input.pdiChoices)

  -- Convert LLM choices to Question DSL
  let choices = map toChoice input.pdiChoices
      question = Q.ProposeDisposition
        { Q.pdItem = input.pdiItem
        , Q.pdChoices = choices
        , Q.pdFallback = input.pdiFallback
        }

  -- Ask user via Question effect
  answer <- requestQuestion question

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
      , Q.choiceReveals = Nothing
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
  :: ( Emit TidyingToolEvent :> es
     , QuestionUI :> es
     )
  => AskSpaceFunctionInput
  -> Eff es AskSpaceFunctionResult
executeAskSpaceFunction _input = do
  -- Use the smart constructor from Question.hs
  answer <- requestQuestion askFunction

  case answer of
    Q.ChoiceAnswer value -> do
      emit $ FunctionChosen value
      pure AskSpaceFunctionResult { asfrFunction = value }

    Q.TextAnswer custom -> do
      emit $ FunctionChosen custom
      pure AskSpaceFunctionResult { asfrFunction = custom }

    _ ->
      -- Fallback
      pure AskSpaceFunctionResult { asfrFunction = "living" }

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
  :: ( Emit TidyingToolEvent :> es
     , QuestionUI :> es
     )
  => ConfirmDoneInput
  -> Eff es ConfirmDoneResult
executeConfirmDone input = do
  let prompt = maybe
        ("Done for now? (" <> T.pack (show input.cdiItemsProcessed) <> " items handled)")
        id
        input.cdiMessage

  answer <- requestQuestion $ Q.Confirm
    { Q.cfPrompt = prompt
    , Q.cfDefault = True  -- Default to "yes, done"
    }

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
-- Note: Requires QuestionUI effect in addition to standard tool effects
makeTidyingDispatcher
  :: ( State SessionState :> es
     , Emit TidyingToolEvent :> es
     , RequestInput :> es
     , Random :> es
     , QuestionUI :> es
     )
  => Text   -- ^ Tool name
  -> Value  -- ^ Tool input
  -> Eff es (Either Text Value)
makeTidyingDispatcher "propose_disposition" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse propose_disposition input: " <> T.pack err
    Success parsed -> do
      result <- executeProposeDisposition parsed
      pure $ Right $ toJSON result

makeTidyingDispatcher "ask_space_function" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse ask_space_function input: " <> T.pack err
    Success parsed -> do
      result <- executeAskSpaceFunction parsed
      pure $ Right $ toJSON result

makeTidyingDispatcher "confirm_done" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse confirm_done input: " <> T.pack err
    Success parsed -> do
      result <- executeConfirmDone parsed
      pure $ Right $ toJSON result

makeTidyingDispatcher name _ =
  pure $ Left $ "Unknown tidying tool: " <> name
