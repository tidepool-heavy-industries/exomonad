{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Domain types for the Habitica routing graph.
--
-- These types are shared between HabiticaRoutingGraph (handlers) and
-- HabiticaTemplates (rendering), extracted here to avoid circular imports.
module Tidepool.Wasm.HabiticaTypes
  ( -- * Input Types
    RawInput(..)
  , ExtractedTask(..)

    -- * Habitica Types
  , HabiticaTodo(..)
  , ExistingTodos(..)

    -- * Matching Types
  , MatchResult(..)

    -- * Suggestion Types
  , Suggestion(..)
  , SuggestionAction(..)
  , UserConfirmation(..)

    -- * Result Types
  , ExecutionResult(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)


-- ============================================================================
-- Input Types
-- ============================================================================

-- | Raw unstructured text input from Telegram.
newtype RawInput = RawInput { unRawInput :: Text }
  deriving stock (Show, Eq, Generic)

-- | A task extracted from unstructured text.
data ExtractedTask = ExtractedTask
  { etDescription :: Text       -- ^ The task description
  , etContext     :: Maybe Text -- ^ Optional context/notes
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExtractedTask where
  toJSON et = object
    [ "description" .= et.etDescription
    , "context" .= et.etContext
    ]

instance FromJSON ExtractedTask where
  parseJSON = withObject "ExtractedTask" $ \v -> ExtractedTask
    <$> v .: "description"
    <*> v .:? "context"


-- ============================================================================
-- Habitica Types
-- ============================================================================

-- | A Habitica todo (simplified for matching).
data HabiticaTodo = HabiticaTodo
  { htId        :: Text          -- ^ Habitica task ID
  , htTitle     :: Text          -- ^ Task title
  , htChecklist :: [Text]        -- ^ Existing checklist items
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON HabiticaTodo where
  toJSON ht = object
    [ "id" .= ht.htId
    , "title" .= ht.htTitle
    , "checklist" .= ht.htChecklist
    ]

instance FromJSON HabiticaTodo where
  parseJSON = withObject "HabiticaTodo" $ \v -> HabiticaTodo
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "checklist"

-- | Existing todos fetched from Habitica.
data ExistingTodos = ExistingTodos
  { etTask  :: ExtractedTask     -- ^ The extracted task (passed through)
  , etTodos :: [HabiticaTodo]    -- ^ Existing Habitica todos
  }
  deriving stock (Show, Eq, Generic)


-- ============================================================================
-- Matching Types
-- ============================================================================

-- | Result of LLM matching task against existing todos.
data MatchResult = MatchResult
  { mrTask     :: ExtractedTask        -- ^ The extracted task
  , mrTodos    :: [HabiticaTodo]       -- ^ All existing todos (for reference)
  , mrMatch    :: Maybe HabiticaTodo   -- ^ Best matching todo, if any
  , mrReason   :: Text                 -- ^ LLM explanation for the match/no-match
  }
  deriving stock (Show, Eq, Generic)


-- ============================================================================
-- Suggestion Types
-- ============================================================================

-- | What action to suggest to the user.
data SuggestionAction
  = SuggestNewTodo          -- ^ Create a new todo
  | SuggestChecklist Text   -- ^ Add to existing todo (todo ID)
  deriving stock (Show, Eq, Generic)

instance ToJSON SuggestionAction where
  toJSON SuggestNewTodo = object ["type" .= ("new_todo" :: Text)]
  toJSON (SuggestChecklist tid) = object
    [ "type" .= ("checklist" :: Text)
    , "todo_id" .= tid
    ]

-- | A formatted suggestion to present to the user.
data Suggestion = Suggestion
  { sgTask           :: ExtractedTask     -- ^ The extracted task
  , sgAction         :: SuggestionAction  -- ^ What to do
  , sgMessage        :: Text              -- ^ Human-readable suggestion message
  , sgMatchingTodo   :: Maybe HabiticaTodo -- ^ The matching todo, if any
  , sgFeedback       :: Maybe Text        -- ^ User feedback from previous attempt
  }
  deriving stock (Show, Eq, Generic)

-- | User's response to a suggestion.
data UserConfirmation
  = Approved                    -- ^ User clicked Yes
  | Denied Text                 -- ^ User clicked No with feedback
  | Skipped                     -- ^ User clicked Skip
  deriving stock (Show, Eq, Generic)


-- ============================================================================
-- Result Types
-- ============================================================================

-- | Result of executing the Habitica action.
data ExecutionResult = ExecutionResult
  { erSuccess :: Bool     -- ^ Whether the action succeeded
  , erMessage :: Text     -- ^ Human-readable result message
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExecutionResult where
  toJSON er = object
    [ "success" .= er.erSuccess
    , "message" .= er.erMessage
    ]
