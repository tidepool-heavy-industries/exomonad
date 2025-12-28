{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Semantic Capture Pipeline for Tidepool
--
-- This module provides infrastructure for building "capture agents" that:
--
-- * Route natural language input to multiple typed destinations
-- * Support propose/correct interaction (user can correct classifications)
-- * Learn from corrections to improve over time
-- * Execute effects (Habitica, Obsidian, etc.) based on routing
--
-- Unlike Machine (conversational state machines), Capture is for
-- run-to-completion pipelines with optional user feedback loops.
--
-- Example flow:
--
-- > "buy milk, call Sarah tomorrow, learned about effect handlers"
-- >
-- > Proposing:
-- >   milk → Groceries
-- >   call Sarah → Calendar (when?)
-- >   effect handlers → Knowledge
-- >
-- > User: "2pm, and effect handlers goes to tidepool-dev"
-- >
-- > ✓ milk → Groceries
-- > ✓ call Sarah → Calendar @ 2pm
-- > ✓ effect handlers → tidepool-dev
-- > (learned: Haskell/effects topics → tidepool-dev)
--
module Tidepool.Capture
  ( -- * Destination Configuration
    Destination(..)
  , CaptureResult(..)

    -- * User Context (accumulated learning)
  , UserContext(..)
  , emptyContext

    -- * Proposal/Feedback Cycle
  , Proposal(..)
  , ProposedItem(..)
  , ParsedFeedback(..)

    -- * Learning
  , RoutingPattern(..)
  , PatternSource(..)
  , EntityInfo(..)

    -- * Capture Effects
  , CaptureEffects

    -- * Running Captures
  , runCapture
  , runCaptureWithFeedback
  , CaptureConfig(..)

    -- * Logging for Meta-Learning
  , CaptureLog(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, FromJSON, ToJSON, object)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Effectful

import Tidepool.Effect
  ( LLM, State, RequestInput, Log
  , llmCall, get, modify, requestText
  , logInfo, logDebug
  )

-- ══════════════════════════════════════════════════════════════
-- DESTINATION
-- ══════════════════════════════════════════════════════════════

-- | A destination for captured content
--
-- Destinations are the "where things go" configuration. Each destination:
--
-- * Has a description (LLM reads this to decide if input applies)
-- * Has formatting instructions (how to format content for this destination)
-- * Has an execute function (actually files the content)
--
-- The execute function receives:
-- * The full user context (for semantic matching)
-- * The original input (for context in formatting)
-- * The entity refs routed to this destination
--
data Destination es = Destination
  { destName        :: Text
    -- ^ Name of destination (e.g., "groceries", "calendar", "knowledge")
  , destDescription :: Text
    -- ^ LLM reads this to decide if content belongs here
    -- e.g., "Food items to buy at the store"
  , destFormatPrompt :: Text
    -- ^ Instructions for formatting content for this destination
    -- e.g., "Just item name, lowercase" or "Full context with topic"
  , destMatchExisting :: Bool
    -- ^ If True, try to semantically match against existing items
    -- e.g., "milk" matches "Groceries" todo, gets added as checklist item
  , destExecute :: UserContext -> Text -> [Text] -> Eff es [CaptureResult]
    -- ^ Execute: context -> original input -> entity refs -> results
  }

-- | Result of capturing an item to a destination
data CaptureResult = CaptureResult
  { crEntity      :: Text
    -- ^ What was captured (e.g., "milk", "call Sarah")
  , crDestination :: Text
    -- ^ Where it went (e.g., "groceries", "calendar")
  , crAsType      :: Text
    -- ^ Type of item created (e.g., "checklist_item", "event", "note")
  , crRef         :: Maybe Text
    -- ^ Reference/link if available (e.g., "todo-abc", "Haskell.md")
  }
  deriving (Show, Eq, Generic)

instance ToJSON CaptureResult
instance FromJSON CaptureResult

-- ══════════════════════════════════════════════════════════════
-- USER CONTEXT (accumulated learning)
-- ══════════════════════════════════════════════════════════════

-- | Information about a known entity (person, project, etc.)
data EntityInfo = EntityInfo
  { eiType        :: Text
    -- ^ Type of entity: "person", "project", "topic"
  , eiDefaultDest :: Maybe Text
    -- ^ Default destination for this entity
  , eiMetadata    :: Map Text Value
    -- ^ Additional info (e.g., scheduling prefs for people)
  }
  deriving (Show, Eq, Generic)

instance ToJSON EntityInfo
instance FromJSON EntityInfo

-- | A routing pattern learned from user corrections
data RoutingPattern = RoutingPattern
  { rpMatch       :: Text
    -- ^ What this pattern matches (e.g., "Rust topics", "calls with Sarah")
  , rpDestination :: Text
    -- ^ Where matching content should go
  , rpSource      :: PatternSource
    -- ^ How this pattern was learned
  , rpConfidence  :: Double
    -- ^ How confident we are (increases with repeated corrections)
  }
  deriving (Show, Eq, Generic)

instance ToJSON RoutingPattern
instance FromJSON RoutingPattern

-- | How a pattern was learned
data PatternSource
  = Explicit    -- ^ User explicitly stated the rule
  | Inferred    -- ^ Inferred from user corrections
  | Configured  -- ^ Configured by user in config file
  deriving (Show, Eq, Generic)

instance ToJSON PatternSource
instance FromJSON PatternSource

-- | User context accumulated over time
--
-- This is the "insight" - what makes the agent smart about YOUR setup.
-- Gets updated after each capture based on corrections.
--
data UserContext = UserContext
  { ucKnownEntities  :: Map Text EntityInfo
    -- ^ Known entities: "Sarah" → person with scheduling prefs
  , ucPatterns       :: [RoutingPattern]
    -- ^ Learned routing patterns
  , ucRecentCaptures :: [CaptureResult]
    -- ^ Recent captures (for context)
  , ucOrgSystem      :: Value
    -- ^ User's org system config (JSON blob)
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserContext
instance FromJSON UserContext

-- | Empty starting context
emptyContext :: UserContext
emptyContext = UserContext
  { ucKnownEntities = Map.empty
  , ucPatterns = []
  , ucRecentCaptures = []
  , ucOrgSystem = object []
  }

-- ══════════════════════════════════════════════════════════════
-- PROPOSAL / FEEDBACK CYCLE
-- ══════════════════════════════════════════════════════════════

-- | A proposed item routing
data ProposedItem = ProposedItem
  { piEntity       :: Text
    -- ^ The entity extracted from input
  , piDestination  :: Text
    -- ^ Where we propose to route it
  , piNeedsClarify :: Maybe Text
    -- ^ What clarification is needed, if any
    -- e.g., "when?" for calendar items
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProposedItem
instance FromJSON ProposedItem

-- | A proposal of how input will be routed
data Proposal = Proposal
  { propItems :: [ProposedItem]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Proposal
instance FromJSON Proposal

-- | Parsed user feedback on a proposal
--
-- The LLM parses natural language feedback into this structure.
-- e.g., "2pm, and effect handlers goes to tidepool-dev" becomes:
--
-- > ParsedFeedback
-- >   { pfApproved = ["milk"]
-- >   , pfCorrections = [("effect handlers", "tidepool-dev")]
-- >   , pfClarifications = [("call Sarah", {"time": "14:00"})]
-- >   , pfLearning = [RoutingPattern "Haskell/effects" "tidepool-dev" Inferred 0.5]
-- >   }
--
data ParsedFeedback = ParsedFeedback
  { pfApproved       :: [Text]
    -- ^ Entities user approved (implicitly or explicitly)
  , pfCorrections    :: [(Text, Text)]
    -- ^ Corrections: (entity, correct destination)
  , pfClarifications :: [(Text, Value)]
    -- ^ Clarifications provided: (entity, extra info)
  , pfLearning       :: [RoutingPattern]
    -- ^ Patterns to learn from this feedback
  }
  deriving (Show, Eq, Generic)

instance ToJSON ParsedFeedback
instance FromJSON ParsedFeedback

-- ══════════════════════════════════════════════════════════════
-- CAPTURE EFFECTS
-- ══════════════════════════════════════════════════════════════

-- | The effect constraints for capture operations
--
-- This is polymorphic so users can add their own domain effects
-- (Habitica, Obsidian, etc.) to the stack.
--
type CaptureEffects es =
  ( LLM :> es
  , State UserContext :> es
  , RequestInput :> es
  , Log :> es
  )

-- ══════════════════════════════════════════════════════════════
-- RUNNING CAPTURES
-- ══════════════════════════════════════════════════════════════

-- | Configuration for running captures
data CaptureConfig es = CaptureConfig
  { ccDestinations     :: [Destination es]
    -- ^ Available destinations
  , ccClassifyPrompt   :: [Destination es] -> UserContext -> Text
    -- ^ Build the classification system prompt
  , ccFeedbackPrompt   :: Proposal -> Text
    -- ^ Build the feedback parsing system prompt
  , ccClassifySchema   :: Value
    -- ^ JSON schema for classification output
  , ccFeedbackSchema   :: Value
    -- ^ JSON schema for feedback parsing output
  }

-- | Run a single capture without user feedback
--
-- Use this for automated pipelines or when you trust classification.
-- For interactive use with corrections, use 'runCaptureWithFeedback'.
--
runCapture
  :: forall es. CaptureEffects es
  => CaptureConfig es
  -> Text                           -- ^ User input
  -> Eff es [CaptureResult]
runCapture config input = do
  ctx <- get @UserContext

  logDebug "[Capture] Classifying input"

  -- Phase 1: Classify
  let classifyPrompt = config.ccClassifyPrompt config.ccDestinations ctx
  proposal <- llmCall @Proposal classifyPrompt input config.ccClassifySchema

  logInfo $ "[Capture] Classified " <> T.pack (show (length proposal.propItems)) <> " items"

  -- Phase 2: Execute each destination group
  executeProposal config ctx input proposal

-- | Run capture with propose/correct feedback loop
--
-- This is the interactive version:
-- 1. Classify input into proposal
-- 2. Present proposal to user
-- 3. Get natural language feedback
-- 4. Parse feedback (LLM)
-- 5. Apply corrections and execute
-- 6. Learn from corrections
--
runCaptureWithFeedback
  :: forall es. CaptureEffects es
  => CaptureConfig es
  -> Text                           -- ^ User input
  -> (Proposal -> Eff es ())        -- ^ Display proposal to user
  -> Eff es ([CaptureResult], [RoutingPattern])
runCaptureWithFeedback config input displayProposal = do
  ctx <- get @UserContext

  logDebug "[Capture] Classifying input"

  -- Phase 1: Classify
  let classifyPrompt = config.ccClassifyPrompt config.ccDestinations ctx
  proposal <- llmCall @Proposal classifyPrompt input config.ccClassifySchema

  logInfo $ "[Capture] Proposing " <> T.pack (show (length proposal.propItems)) <> " items"

  -- Phase 2: Present to user
  displayProposal proposal

  -- Phase 3: Get feedback
  feedback <- requestText "Approve, correct, or add details:"

  -- Phase 4: Parse feedback (LLM understands natural language corrections)
  let feedbackPrompt = config.ccFeedbackPrompt proposal
  parsed <- llmCall @ParsedFeedback feedbackPrompt feedback config.ccFeedbackSchema

  logInfo $ "[Capture] Parsed feedback: "
    <> T.pack (show (length parsed.pfCorrections)) <> " corrections, "
    <> T.pack (show (length parsed.pfClarifications)) <> " clarifications"

  -- Phase 5: Apply corrections and execute
  let correctedProposal = applyCorrections proposal parsed
  results <- executeProposal config ctx input correctedProposal

  -- Phase 6: Learn from corrections
  let newPatterns = parsed.pfLearning
  modify @UserContext $ \c -> c
    { ucPatterns = newPatterns ++ c.ucPatterns
    , ucRecentCaptures = results ++ take 10 c.ucRecentCaptures
    }

  pure (results, newPatterns)

-- | Apply corrections to a proposal
applyCorrections :: Proposal -> ParsedFeedback -> Proposal
applyCorrections proposal feedback = Proposal
  { propItems = map correctItem proposal.propItems
  }
  where
    corrections = Map.fromList feedback.pfCorrections

    correctItem item = case Map.lookup item.piEntity corrections of
      Just newDest -> item { piDestination = newDest }
      Nothing -> item

-- | Execute a proposal, routing to destinations
executeProposal
  :: forall es. CaptureEffects es
  => CaptureConfig es
  -> UserContext
  -> Text                    -- ^ Original input
  -> Proposal
  -> Eff es [CaptureResult]
executeProposal config ctx originalInput proposal = do
  -- Group items by destination
  let grouped = groupByDest proposal.propItems

  -- Execute each destination
  results <- concat <$> mapM (executeDestGroup config ctx originalInput) grouped

  pure results

-- | Group proposed items by destination
groupByDest :: [ProposedItem] -> [(Text, [Text])]
groupByDest items =
  Map.toList $ foldr addItem Map.empty items
  where
    addItem item = Map.insertWith (++) item.piDestination [item.piEntity]

-- | Execute a group of items for one destination
executeDestGroup
  :: forall es. CaptureEffects es
  => CaptureConfig es
  -> UserContext
  -> Text                    -- ^ Original input
  -> (Text, [Text])          -- ^ (destination name, entities)
  -> Eff es [CaptureResult]
executeDestGroup config ctx originalInput (destName, entities) = do
  case findDest config.ccDestinations destName of
    Nothing -> do
      logInfo $ "[Capture] Unknown destination: " <> destName
      pure []
    Just dest -> do
      logDebug $ "[Capture] Executing " <> destName <> " with " <> T.pack (show entities)
      dest.destExecute ctx originalInput entities

-- | Find a destination by name
findDest :: [Destination es] -> Text -> Maybe (Destination es)
findDest dests name = go dests
  where
    go [] = Nothing
    go (d:ds)
      | d.destName == name = Just d
      | otherwise = go ds

-- ══════════════════════════════════════════════════════════════
-- LOGGING FOR META-LEARNING
-- ══════════════════════════════════════════════════════════════

-- | Log entry for meta-learning
--
-- These logs get read by the "backend Claude" to update configuration:
-- * Adjust destination descriptions
-- * Add new routing patterns
-- * Update entity database
--
data CaptureLog = CaptureLog
  { clInput      :: Text
    -- ^ Original user input
  , clProposed   :: Proposal
    -- ^ What was proposed
  , clFeedback   :: Maybe ParsedFeedback
    -- ^ User feedback (if interactive)
  , clResults    :: [CaptureResult]
    -- ^ What was actually captured
  , clTimestamp  :: UTCTime
    -- ^ When this happened
  }
  deriving (Show, Eq, Generic)

instance ToJSON CaptureLog
instance FromJSON CaptureLog
