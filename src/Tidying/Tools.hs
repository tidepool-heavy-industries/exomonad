{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Tidying Tools (mid-turn LLM capabilities)
--
-- = Tool Categories
--
-- == Regular Tools (ToolSuccess)
-- Execute an action, return result, LLM continues:
-- - propose_disposition: Propose item disposition, get user confirmation
-- - ask_space_function: Ask what the space is for
--
-- == Transition Tools (ToolBreak)
-- Change mode, end turn, start fresh:
-- - begin_sorting: Surveying → Sorting
-- - need_to_clarify: Sorting → Clarifying
-- - user_seems_stuck: Sorting → DecisionSupport
-- - time_to_wrap: Sorting → WindingDown
-- - resume_sorting: Clarifying/DecisionSupport → Sorting
-- - skip_item: Clarifying → Sorting
-- - end_session: WindingDown → (end)
--
-- = Effect Architecture
--
-- Tools use IO callbacks (QuestionHandler) rather than the QuestionUI effect
-- because tools run inside the LLM interpreter, after QuestionUI has been
-- interpreted. The QuestionHandler is passed to the dispatcher at setup time.
--
module Tidying.Tools
  ( -- * Regular Tools
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

    -- * Transition tool inputs
  , NeedToClarifyInput(..)
  , UserSeemsStuckInput(..)

    -- * Tool Registration
  , tidyingTools
  , toolsForMode
  , makeTidyingDispatcher
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON(..), FromJSON(..), Result(..), object, (.=), (.:), (.:?), (.!=), withObject, fromJSON)
import GHC.Generics (Generic)
import Effectful (Eff, (:>), IOE)

import Tidepool.Effect
  ( State, Emit, Log
  , emit, get, logInfo, logWarn, modify
  , QuestionHandler
  , ToolResult(..)
  )
import Tidepool.Schema (objectSchema, arraySchema, emptySchema, schemaToValue, describeField, SchemaType(..))
import Tidying.Question as Q
import Tidying.State
  ( SessionState(..), Mode(..)
  , SortingData(..), ClarifyingData(..), DecisionSupportData(..), WindingDownData(..)
  )
import Tidying.Events (TidyingEvent(..))
import Tidying.Types (SpaceFunction(..), ItemName(..))

-- ══════════════════════════════════════════════════════════════
-- PROPOSE DISPOSITION TOOL (Regular)
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
  deriving (Show, Eq, Generic, ToJSON)

-- | Custom FromJSON to match schema field names (item, choices, fallback)
instance FromJSON ProposeDispositionInput where
  parseJSON = withObject "ProposeDispositionInput" $ \o -> ProposeDispositionInput
    <$> o .: "item"
    <*> o .: "choices"
    <*> o .:? "fallback"

-- | Result returned to LLM
data ProposeDispositionResult = ProposeDispositionResult
  { pdrDisposition :: Q.ItemDisposition  -- ^ Where the item goes
  , pdrUserResponse :: Text               -- ^ What user said (for LLM to interpret)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Schema for LLM
proposeDispositionSchema :: Value
proposeDispositionSchema = schemaToValue $ objectSchema
  [ ("item", describeField "item" "SPECIFIC description: location + appearance. E.g., 'the blue mug on the stack of papers by the monitor' or 'tangled black cables behind the keyboard'. Include enough detail that user can unambiguously identify it." (emptySchema TString))
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
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure ProposeDispositionResult
        { pdrDisposition = Q.SkipForNow
        , pdrUserResponse = "(no response - error)"
        }

    Right answer ->
      case answer of
        Q.DispositionAnswer disp -> do
          let label = dispositionToLabel disp
          emit $ UserConfirmed input.pdiItem label
          pure ProposeDispositionResult
            { pdrDisposition = disp
            , pdrUserResponse = label
            }

        Q.TextAnswer txt -> do
          emit $ UserCorrected input.pdiItem txt
          pure ProposeDispositionResult
            { pdrDisposition = Q.PlaceAt txt
            , pdrUserResponse = txt
            }

        _ -> do
          pure ProposeDispositionResult
            { pdrDisposition = Q.SkipForNow
            , pdrUserResponse = "(skipped)"
            }

  where
    toChoice :: ProposedChoice -> Q.Choice
    toChoice pc = Q.Choice
      { Q.choiceLabel = pc.pcLabel
      , Q.choiceValue = parseDisposition pc
      , Q.choiceReveals = []  -- ProposeDisposition doesn't use reveals (yet)
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

    dispositionToLabel :: Q.ItemDisposition -> Text
    dispositionToLabel Q.Trash = "trash"
    dispositionToLabel Q.Donate = "donate"
    dispositionToLabel Q.Recycle = "recycle"
    dispositionToLabel Q.SkipForNow = "skip"
    dispositionToLabel Q.NeedMoreInfo = "need more info"
    dispositionToLabel (Q.PlaceAt loc) = "place at " <> loc

-- ══════════════════════════════════════════════════════════════
-- ASK SPACE FUNCTION TOOL (Regular)
-- ══════════════════════════════════════════════════════════════

data AskSpaceFunction = AskSpaceFunction
  deriving (Show, Eq, Generic)

data AskSpaceFunctionInput = AskSpaceFunctionInput
  { asfiPrompt :: Text
  , asfiChoices :: [SpaceFunctionChoice]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AskSpaceFunctionInput where
  parseJSON = withObject "AskSpaceFunctionInput" $ \o -> AskSpaceFunctionInput
    <$> o .: "prompt"
    <*> o .: "choices"

data SpaceFunctionChoice = SpaceFunctionChoice
  { sfcLabel :: Text
  , sfcValue :: Text
  , sfcReveals :: [Q.Question]  -- Follow-up questions when this option selected
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpaceFunctionChoice where
  toJSON c = object $
    [ "label" .= c.sfcLabel
    , "value" .= c.sfcValue
    ] <> case c.sfcReveals of
           [] -> []
           rs -> ["reveals" .= rs]

instance FromJSON SpaceFunctionChoice where
  parseJSON = withObject "SpaceFunctionChoice" $ \o ->
    SpaceFunctionChoice
      <$> o .: "label"
      <*> o .: "value"
      <*> o .:? "reveals" .!= []

data AskSpaceFunctionResult = AskSpaceFunctionResult
  { asfrFunction :: Text              -- ^ Primary answer (first in path, or single answer)
  , asfrAnswerPath :: [(Text, Text)]  -- ^ All answers from tree walk: [(questionId, value)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

askSpaceFunctionSchema :: Value
askSpaceFunctionSchema = schemaToValue $ objectSchema
  [ ("prompt", describeField "prompt" "Question to ask (e.g., 'What do you use this space for?')" (emptySchema TString))
  , ("choices", describeField "choices" "2-4 options. EACH MUST have label, value, AND reveals array."
      (arraySchema $ objectSchema
        [ ("label", emptySchema TString)
        , ("value", emptySchema TString)
        , ("reveals", describeField "reveals" "REQUIRED: Array of follow-up questions. Use [] for leaf nodes, or [{\"choose\":{...}}] for branches." (arraySchema (emptySchema TObject)))
        ]
        ["label", "value", "reveals"]))  -- reveals is now required!
  ]
  ["prompt", "choices"]

executeAskSpaceFunction
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler
  -> AskSpaceFunctionInput
  -> Eff es AskSpaceFunctionResult
executeAskSpaceFunction askQuestion input = do
  logInfo $ "TOOL ask_space_function: prompt=" <> input.asfiPrompt

  -- Warn if no reveals - LLM should be building a tree, not a flat list
  let allFlat = all (\c -> null c.sfcReveals) input.asfiChoices
  when allFlat $
    logWarn "ask_space_function: No reveals provided. Consider adding follow-up questions for anchors."

  let choiceOptions = map (\c -> Q.ChoiceOption c.sfcLabel c.sfcValue c.sfcReveals) input.asfiChoices
      question = Q.Choose
        { Q.chPrompt = input.asfiPrompt
        , Q.chId = "function"
        , Q.chChoices = choiceOptions
        }

  result <- liftIO $ try @SomeException $ askQuestion question

  case result of
    Left err -> do
      logWarn $ "Question handler failed: " <> T.pack (show err)
      pure AskSpaceFunctionResult { asfrFunction = "living", asfrAnswerPath = [] }

    Right answer -> do
      case answer of
        Q.ChoiceAnswer value -> do
          emit $ FunctionChosen value
          modify @SessionState $ \st -> st { spaceFunction = Just (SpaceFunction value) }
          pure AskSpaceFunctionResult { asfrFunction = value, asfrAnswerPath = [("function", value)] }

        Q.TextAnswer custom -> do
          emit $ FunctionChosen custom
          modify @SessionState $ \st -> st { spaceFunction = Just (SpaceFunction custom) }
          pure AskSpaceFunctionResult { asfrFunction = custom, asfrAnswerPath = [("function", custom)] }

        Q.AnswerPath path -> do
          -- Parse path: first answer is function, "anchor" answers are anchors
          let functionValue = case lookup "function" path of
                Just v -> v
                Nothing -> case path of
                  ((_, v):_) -> v  -- fallback to first answer
                  [] -> "living"
              anchorValues = [v | (k, v) <- path, k == "anchor"]

          emit $ FunctionChosen functionValue
          modify @SessionState $ \st -> st
            { spaceFunction = Just (SpaceFunction functionValue)
            , anchors = st.anchors ++ map ItemName anchorValues
            }
          logInfo $ "SURVEY: function=" <> functionValue <> ", anchors=" <> T.intercalate "," anchorValues
          pure AskSpaceFunctionResult { asfrFunction = functionValue, asfrAnswerPath = path }

        _ -> do
          pure AskSpaceFunctionResult { asfrFunction = "living", asfrAnswerPath = [] }

-- ══════════════════════════════════════════════════════════════
-- CONFIRM DONE TOOL (Regular)
-- ══════════════════════════════════════════════════════════════

data ConfirmDone = ConfirmDone
  deriving (Show, Eq, Generic)

data ConfirmDoneInput = ConfirmDoneInput
  { cdiItemsProcessed :: Int
  , cdiMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfirmDoneInput where
  parseJSON = withObject "ConfirmDoneInput" $ \o -> ConfirmDoneInput
    <$> o .: "items_processed"
    <*> o .:? "message"

data ConfirmDoneResult = ConfirmDoneResult
  { cdrConfirmed :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

confirmDoneSchema :: Value
confirmDoneSchema = schemaToValue $ objectSchema
  [ ("items_processed", describeField "items_processed" "Number of items handled this session" (emptySchema TNumber))
  , ("message", describeField "message" "Optional message to show" (emptySchema TString))
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
    , Q.cfDefault = True
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
-- TRANSITION TOOLS (ToolBreak)
-- ══════════════════════════════════════════════════════════════

-- | Input for need_to_clarify transition tool
data NeedToClarifyInput = NeedToClarifyInput
  { ntcItem :: Text           -- ^ Item to clarify
  , ntcPhotoContext :: Text   -- ^ What we observed in photo
  , ntcReason :: Text         -- ^ Why user is confused
  }
  deriving (Show, Eq, Generic)

instance FromJSON NeedToClarifyInput where
  parseJSON = withObject "NeedToClarifyInput" $ \o -> NeedToClarifyInput
    <$> o .: "item"
    <*> o .: "photo_context"
    <*> o .: "reason"

instance ToJSON NeedToClarifyInput where
  toJSON NeedToClarifyInput{..} = object
    [ "item" .= ntcItem
    , "photo_context" .= ntcPhotoContext
    , "reason" .= ntcReason
    ]

needToClarifySchema :: Value
needToClarifySchema = schemaToValue $ objectSchema
  [ ("item", describeField "item" "What item we're clarifying" (emptySchema TString))
  , ("photo_context", describeField "photo_context" "What we observed in the photo (location, nearby objects)" (emptySchema TString))
  , ("reason", describeField "reason" "Why user is confused (their response)" (emptySchema TString))
  ]
  ["item", "photo_context", "reason"]

-- | Input for user_seems_stuck transition tool
data UserSeemsStuckInput = UserSeemsStuckInput
  { ussStuckItem :: Text  -- ^ Item they're stuck on
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserSeemsStuckInput where
  parseJSON = withObject "UserSeemsStuckInput" $ \o -> UserSeemsStuckInput
    <$> o .: "stuck_item"

instance ToJSON UserSeemsStuckInput where
  toJSON UserSeemsStuckInput{..} = object
    [ "stuck_item" .= ussStuckItem
    ]

userSeemsStuckSchema :: Value
userSeemsStuckSchema = schemaToValue $ objectSchema
  [ ("stuck_item", describeField "stuck_item" "Item the user is stuck deciding about" (emptySchema TString))
  ]
  ["stuck_item"]

-- | Execute begin_sorting: Surveying → Sorting
executeBeginSorting
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => Eff es ToolResult
executeBeginSorting = do
  oldMode <- (.mode) <$> get @SessionState
  let newMode = Sorting $ SortingData Nothing Nothing
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak "[Continue as: Sorting. Begin processing items.]"

-- | Execute need_to_clarify: Sorting → Clarifying
executeNeedToClarify
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => NeedToClarifyInput
  -> Eff es ToolResult
executeNeedToClarify input = do
  oldMode <- (.mode) <$> get @SessionState
  let newModeData = ClarifyingData
        { cdItem = input.ntcItem
        , cdPhotoContext = input.ntcPhotoContext
        , cdReason = input.ntcReason
        }
      newMode = Clarifying newModeData
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak $ "[Continue as: Clarifying. Describe: " <> input.ntcItem <> "]"

-- | Execute user_seems_stuck: Sorting → DecisionSupport
executeUserSeemsStuck
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => UserSeemsStuckInput
  -> Eff es ToolResult
executeUserSeemsStuck input = do
  oldMode <- (.mode) <$> get @SessionState
  let newModeData = DecisionSupportData { dsdStuckItem = input.ussStuckItem }
      newMode = DecisionSupport newModeData
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak $ "[Continue as: DecisionSupport. Help with: " <> input.ussStuckItem <> "]"

-- | Execute time_to_wrap: Sorting → WindingDown
executeTimeToWrap
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => Eff es ToolResult
executeTimeToWrap = do
  oldMode <- (.mode) <$> get @SessionState
  let newMode = WindingDown $ WindingDownData Nothing []
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak "[Continue as: WindingDown. Wrap up session.]"

-- | Execute resume_sorting: Clarifying/DecisionSupport → Sorting
executeResumeSorting
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => Eff es ToolResult
executeResumeSorting = do
  oldMode <- (.mode) <$> get @SessionState
  let newMode = Sorting $ SortingData Nothing Nothing
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak "[Continue as: Sorting. Resume item processing.]"

-- | Execute skip_item: Clarifying → Sorting
executeSkipItem
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => Eff es ToolResult
executeSkipItem = do
  oldMode <- (.mode) <$> get @SessionState
  let newMode = Sorting $ SortingData Nothing Nothing
  modify @SessionState $ \st -> st { mode = newMode }
  emit $ ModeChanged oldMode newMode
  pure $ ToolBreak "[Continue as: Sorting. Item skipped, move on.]"

-- | Execute end_session: WindingDown → (end)
executeEndSession
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     )
  => Eff es ToolResult
executeEndSession = do
  st <- get @SessionState
  emit $ SessionEnded st.itemsProcessed
  pure $ ToolBreak "[Session ended.]"

-- ══════════════════════════════════════════════════════════════
-- TOOL SCHEMAS
-- ══════════════════════════════════════════════════════════════

-- | begin_sorting tool (Surveying → Sorting)
beginSortingTool :: Value
beginSortingTool = object
  [ "name" .= ("begin_sorting" :: Text)
  , "description" .= ("Transition from Surveying to Sorting mode. Call when you've established what the space is for and what stays. This ENDS the current turn." :: Text)
  , "input_schema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "additionalProperties" .= False
      ]
  ]

-- | need_to_clarify tool (Sorting → Clarifying)
needToClarifyTool :: Value
needToClarifyTool = object
  [ "name" .= ("need_to_clarify" :: Text)
  , "description" .= ("User can't identify the item. Transition to Clarifying mode to describe it in more detail using spatial references and physical traits. This ENDS the current turn." :: Text)
  , "input_schema" .= needToClarifySchema
  ]

-- | user_seems_stuck tool (Sorting → DecisionSupport)
userSeemsStuckTool :: Value
userSeemsStuckTool = object
  [ "name" .= ("user_seems_stuck" :: Text)
  , "description" .= ("User is struggling to decide about an item. Transition to DecisionSupport mode to help them through it gently. This ENDS the current turn." :: Text)
  , "input_schema" .= userSeemsStuckSchema
  ]

-- | time_to_wrap tool (Sorting → WindingDown)
timeToWrapTool :: Value
timeToWrapTool = object
  [ "name" .= ("time_to_wrap" :: Text)
  , "description" .= ("Time to wrap up the session. User mentioned being done/tired or you sense natural stopping point. Transition to WindingDown mode. This ENDS the current turn." :: Text)
  , "input_schema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "additionalProperties" .= False
      ]
  ]

-- | resume_sorting tool (Clarifying/DecisionSupport → Sorting)
resumeSortingTool :: Value
resumeSortingTool = object
  [ "name" .= ("resume_sorting" :: Text)
  , "description" .= ("Return to Sorting mode. Call when clarification is complete or user made a decision. This ENDS the current turn." :: Text)
  , "input_schema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "additionalProperties" .= False
      ]
  ]

-- | skip_item tool (Clarifying → Sorting)
skipItemTool :: Value
skipItemTool = object
  [ "name" .= ("skip_item" :: Text)
  , "description" .= ("Skip this item and move on. Use when user still can't identify it after clarification. This ENDS the current turn." :: Text)
  , "input_schema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "additionalProperties" .= False
      ]
  ]

-- | end_session tool (WindingDown → end)
endSessionTool :: Value
endSessionTool = object
  [ "name" .= ("end_session" :: Text)
  , "description" .= ("End the tidying session. This ENDS the session completely." :: Text)
  , "input_schema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "additionalProperties" .= False
      ]
  ]

-- | propose_disposition tool (regular, for Sorting mode)
proposeDispositionTool :: Value
proposeDispositionTool = object
  [ "name" .= ("propose_disposition" :: Text)
  , "description" .= (T.unlines
      [ "Propose where an item should go."
      , ""
      , "ITEM DESCRIPTION: Be SPECIFIC about location + appearance."
      , "Bad: 'the mouse'. Good: 'the white mouse with green logo on the right side of the keyboard'."
      , ""
      , "CHOICES: 2-4 options ranked by likelihood (first = best guess)."
      , ""
      , "USER CONFUSION: If user says 'I don't see it', describe WHERE you see it more precisely."
      ] :: Text)
  , "input_schema" .= proposeDispositionSchema
  ]

-- | ask_space_function tool (regular, for Surveying mode)
askSpaceFunctionTool :: Value
askSpaceFunctionTool = object
  [ "name" .= ("ask_space_function" :: Text)
  , "description" .= (T.unlines
      [ "Build a branching question tree. Gather function+anchor in ONE call."
      , ""
      , "Example with reveals:"
      , "{\"prompt\":\"What's this space for?\", \"choices\":["
      , "  {\"label\":\"Work\", \"value\":\"work\", \"reveals\":[{"
      , "    \"choose\":{\"prompt\":\"Anchor?\", \"id\":\"anchor\", \"choices\":["
      , "      {\"label\":\"Desk\", \"value\":\"desk\", \"reveals\":[]},"
      , "      {\"label\":\"Computer\", \"value\":\"computer\", \"reveals\":[]}]}}]},"
      , "  {\"label\":\"Living\", \"value\":\"living\", \"reveals\":[{"
      , "    \"choose\":{\"prompt\":\"Anchor?\", \"id\":\"anchor\", \"choices\":["
      , "      {\"label\":\"Couch\", \"value\":\"couch\", \"reveals\":[]}]}}]}"
      , "]}"
      , ""
      , "Rules: reveals=[] for leaves, reveals=[{choose:{...}}] for branches."
      ] :: Text)
  , "input_schema" .= askSpaceFunctionSchema
  ]

-- ══════════════════════════════════════════════════════════════
-- TOOL SELECTION BY MODE
-- ══════════════════════════════════════════════════════════════

-- | Select tools available in each mode
toolsForMode :: Mode -> [Value]
toolsForMode (Surveying _) =
  [ askSpaceFunctionTool
  , beginSortingTool
  ]

toolsForMode (Sorting _) =
  [ proposeDispositionTool
  , needToClarifyTool
  , userSeemsStuckTool
  , timeToWrapTool
  ]

toolsForMode (Clarifying _) =
  [ resumeSortingTool
  , skipItemTool
  ]

toolsForMode (DecisionSupport _) =
  [ resumeSortingTool
  ]

toolsForMode (WindingDown _) =
  [ endSessionTool
  ]

-- | All tidying tools (for backwards compatibility)
tidyingTools :: [Value]
tidyingTools =
  [ proposeDispositionTool
  , askSpaceFunctionTool
  , beginSortingTool
  , needToClarifyTool
  , userSeemsStuckTool
  , timeToWrapTool
  , resumeSortingTool
  , skipItemTool
  , endSessionTool
  ]

-- ══════════════════════════════════════════════════════════════
-- TOOL DISPATCHER
-- ══════════════════════════════════════════════════════════════

-- | Create dispatcher for tidying tools
makeTidyingDispatcher
  :: ( State SessionState :> es
     , Emit TidyingEvent :> es
     , Log :> es
     , IOE :> es
     )
  => QuestionHandler
  -> Text
  -> Value
  -> Eff es (Either Text ToolResult)

-- Regular tools (ToolSuccess)
makeTidyingDispatcher handler "propose_disposition" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse propose_disposition input: " <> T.pack err
    Success parsed -> do
      result <- executeProposeDisposition handler parsed
      pure $ Right $ ToolSuccess $ toJSON result

makeTidyingDispatcher handler "ask_space_function" input = do
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

-- Transition tools (ToolBreak)
makeTidyingDispatcher _ "begin_sorting" _ = do
  result <- executeBeginSorting
  pure $ Right result

makeTidyingDispatcher _ "need_to_clarify" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse need_to_clarify input: " <> T.pack err
    Success parsed -> do
      result <- executeNeedToClarify parsed
      pure $ Right result

makeTidyingDispatcher _ "user_seems_stuck" input =
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse user_seems_stuck input: " <> T.pack err
    Success parsed -> do
      result <- executeUserSeemsStuck parsed
      pure $ Right result

makeTidyingDispatcher _ "time_to_wrap" _ = do
  result <- executeTimeToWrap
  pure $ Right result

makeTidyingDispatcher _ "resume_sorting" _ = do
  result <- executeResumeSorting
  pure $ Right result

makeTidyingDispatcher _ "skip_item" _ = do
  result <- executeSkipItem
  pure $ Right result

makeTidyingDispatcher _ "end_session" _ = do
  result <- executeEndSession
  pure $ Right result

makeTidyingDispatcher _ name _ =
  pure $ Left $ "Unknown tidying tool: " <> name
