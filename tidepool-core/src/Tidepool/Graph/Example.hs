{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example graph definitions for the record-based Graph DSL.
--
-- This module contains example graphs using the Servant-style record syntax.
module Tidepool.Graph.Example
  ( -- * Example Graphs (record syntax - Servant-style)
    SupportGraph(..)
  , supportHandlers

    -- * Example Memory Types
  , ExploreMem(..)
  , SessionState(..)

    -- * Example Tools
  , SearchTool(..)
  , SearchInput(..)
  , SearchOutput(..)

    -- * Example Templates
  , ClassifyTpl
  , ClassifyContext(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Effectful (type (:>), Eff)
import Effectful.State.Static.Local (State, get)
import GHC.Generics (Generic)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Types (type (:@), Needs, Schema, Template, UsesEffects)
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G (Entry, Exit, LLMNode, LogicNode, ValidGraphRecord)
import Tidepool.Graph.Goto (Goto, To, GotoChoice, gotoChoice, LLMHandler(..))
import Tidepool.Graph.Reify (ReifyRecordGraph(..), makeGraphInfo)
import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)
import Tidepool.Graph.Example.Context (ClassifyContext(..))
import Tidepool.Schema (HasJSONSchema(..), JSONSchema(..), SchemaType(..), objectSchema, arraySchema, describeField, emptySchema)

-- ════════════════════════════════════════════════════════════════════════════
-- EXAMPLE TYPES
-- ════════════════════════════════════════════════════════════════════════════

data Message = Message { msgContent :: String }
data Intent = IntentRefund | IntentQuestion | IntentComplaint
data Response = Response { respText :: String }

-- ════════════════════════════════════════════════════════════════════════════
-- V2 TOOL EXAMPLE
-- ════════════════════════════════════════════════════════════════════════════

-- | Tool input type for search functionality.
--
-- In production, you would use @deriveHasJSONSchema ''SearchInput@ in a
-- separate module. Here we manually write the instance for demonstration.
data SearchInput = SearchInput
  { query :: Text       -- ^ The search query string
  , maxResults :: Int   -- ^ Maximum number of results to return
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Tool output type for search results.
data SearchOutput = SearchOutput
  { results :: [Text]   -- ^ List of matching results
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Manual HasJSONSchema instances (normally derived via TH)
instance HasJSONSchema SearchInput where
  jsonSchema = objectSchema
    [ ("query", describeField "query" "The search query string" (emptySchema TString))
    , ("maxResults", describeField "maxResults" "Maximum number of results (1-100)" (emptySchema TInteger))
    ]
    ["query", "maxResults"]

instance HasJSONSchema SearchOutput where
  jsonSchema = objectSchema
    [ ("results", describeField "results" "List of matching results" (arraySchema (emptySchema TString)))
    ]
    ["results"]

-- | Example tool: search the knowledge base.
--
-- This demonstrates the unified ToolDef API where everything hangs off
-- the typeclass - types, effects, name, description, and implementation.
data SearchTool = SearchTool
  deriving (Typeable)

instance ToolDef SearchTool where
  type ToolInput SearchTool = SearchInput
  type ToolOutput SearchTool = SearchOutput
  type ToolEffects SearchTool = '[]  -- Pure implementation

  toolName _ = "search"
  toolDescription _ = "Search the knowledge base for relevant information"
  toolExecute _ input =
    -- Stub implementation - in real code this would use effects
    pure SearchOutput { results = ["Result for: " <> input.query] }

-- ════════════════════════════════════════════════════════════════════════════
-- EXAMPLE MEMORY TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Example memory type for a research/exploration node.
--
-- This is the node's private persistent state - it remembers what it has
-- searched for, what worked, and what didn't work.
data ExploreMem = ExploreMem
  { urlsVisited :: [Text]    -- ^ URLs already searched
  , deadEnds :: [Text]       -- ^ Queries that didn't help
  , promisingLeads :: [Text] -- ^ Things worth exploring more
  }

-- | Example global state for a research session.
--
-- This is shared across all nodes in the graph.
data SessionState = SessionState
  { totalSearches :: Int     -- ^ How many searches have been performed
  , sessionNotes :: [Text]   -- ^ Notes accumulated during the session
  }

-- ════════════════════════════════════════════════════════════════════════════
-- EXAMPLE TEMPLATES
-- ════════════════════════════════════════════════════════════════════════════

-- | The template type (phantom type for type-level configuration).
data ClassifyTpl

-- | Phase 1: Compile the template file via TH.
--
-- This validates at compile time that @{{ topic }}@ and @{{ categories }}@
-- exist in ClassifyContext's ToGVal instance.
--
-- Note: ClassifyContext is defined in Example.Context (a separate module)
-- because TH splices require types to be in previously-compiled modules.
classifyCompiled :: TypedTemplate ClassifyContext SourcePos
classifyCompiled = $(typedTemplateFile ''ClassifyContext "templates/example.jinja")

-- | Phase 2: Define the TemplateDef instance.
--
-- This connects:
--
-- * The context type (ClassifyContext)
-- * The effect constraints (State SessionState)
-- * The compiled template
-- * The context builder function
instance TemplateDef ClassifyTpl where
  type TemplateContext ClassifyTpl = ClassifyContext
  type TemplateConstraint ClassifyTpl es = (State SessionState :> es)

  templateName = "classify"
  templateDescription = "Classify user intent into categories"
  templateCompiled = classifyCompiled

  buildContext = do
    st <- get @SessionState
    pure ClassifyContext
      { topic = "user message"  -- In real code: from state/input
      , categories = T.intercalate ", " st.sessionNotes
      }

-- ════════════════════════════════════════════════════════════════════════════
-- RECORD-BASED GRAPH (Servant-style pattern)
-- ════════════════════════════════════════════════════════════════════════════

-- $recordSyntax
--
-- This is the new Servant-inspired pattern for defining graphs. Instead of
-- type-level lists, graphs are mode-parameterized records:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- Entry Message
--   , classify :: mode :- LLM :@ Needs '[Message] :@ Schema Intent
--   , exit     :: mode :- Exit Response
--   }
--   deriving Generic
-- @
--
-- Modes determine interpretation:
--
-- * 'AsGraph' - Identity; fields are node definitions (for validation)
-- * 'AsHandler es' - Fields are handler function types (for execution)
--
-- This pattern provides:
--
-- * Field names become node names (no @\":=\"@ annotation needed)
-- * Type-safe handler records with correct effect constraints
-- * Generic traversal for validation and reification

-- | A customer support graph using the record-based syntax.
--
-- This is equivalent to:
--
-- @
-- type SupportGraphList = Graph
--   '[ Entry :~> Message
--    , "classify" := LLM :@ Needs '[Message] :@ Template ClassifyTpl :@ Schema Intent
--    , "route"    := Logic :@ Needs '[Intent] :@ UsesEffects '[Goto "refund", Goto "faq", Goto Exit]
--    , "refund"   := LLM :@ Needs '[Message] :@ Schema Response
--    , "faq"      := LLM :@ Needs '[Message] :@ Schema Response
--    , Exit :<~ Response
--    ]
-- @
--
-- But with record syntax, field names become node names automatically.
-- | Dummy template for refund handler
data RefundTpl

-- | Dummy template for FAQ handler
data FaqTpl

-- | Dummy context for refund/faq (would have real context in production)
data SimpleContext = SimpleContext { scContent :: Text }

-- Dummy TemplateDef instances for RefundTpl and FaqTpl
-- (In real code, these would have proper templates)
instance TemplateDef RefundTpl where
  type TemplateContext RefundTpl = SimpleContext
  type TemplateConstraint RefundTpl es = ()
  templateName = "refund"
  templateDescription = "Handle refund requests"
  templateCompiled = error "RefundTpl stub"
  buildContext = pure SimpleContext { scContent = "refund" }

instance TemplateDef FaqTpl where
  type TemplateContext FaqTpl = SimpleContext
  type TemplateConstraint FaqTpl es = ()
  templateName = "faq"
  templateDescription = "Answer FAQ questions"
  templateCompiled = error "FaqTpl stub"
  buildContext = pure SimpleContext { scContent = "faq" }

data SupportGraph mode = SupportGraph
  { sgEntry    :: mode :- G.Entry Message
  , sgClassify :: mode :- G.LLMNode :@ Needs '[Message] :@ Template ClassifyTpl :@ Schema Intent
    -- Note: Goto targets must match actual field names for gotoField validation
  , sgRoute    :: mode :- G.LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "sgRefund" Message, Goto "sgFaq" Message]
  , sgRefund   :: mode :- G.LLMNode :@ Needs '[Message] :@ Template RefundTpl :@ Schema Response
  , sgFaq      :: mode :- G.LLMNode :@ Needs '[Message] :@ Template FaqTpl :@ Schema Response
  , sgExit     :: mode :- G.Exit Response
  }
  deriving Generic

-- | Handler record for SupportGraph.
--
-- In 'AsHandler es' mode, the NodeHandler type family computes:
--
-- * Entry -> Proxy Message (marker, not a handler)
-- * LLM :@ Needs '[A] :@ Template T :@ Schema B -> LLMHandler A B '[] es (TemplateContext T)
-- * Logic :@ Needs '[A] :@ UsesEffects effs -> A -> Eff es (GotoChoice targets)
-- * Exit -> Proxy Response (marker, not a handler)
--
-- Note: LLM handlers are wrapped in LLMBefore/LLMAfter/LLMBoth to specify
-- which phases they handle. The runner uses these to orchestrate the LLM call.
supportHandlers :: SupportGraph (AsHandler '[State SessionState])
supportHandlers = SupportGraph
  { sgEntry    = Proxy @Message
  , sgClassify = LLMBefore $ \msg -> do
      -- Before handler: builds context for the classify template
      st <- get @SessionState
      pure ClassifyContext
        { topic = msgContent msg
        , categories = T.intercalate ", " st.sessionNotes
        }
  , sgRoute    = \intent -> do
      -- Logic handler: returns GotoChoice to specify transition
      --
      -- gotoChoice @"sgRefund" validates at compile time that:
      -- 1. To "sgRefund" Message is in the node's allowed targets
      --
      -- The return type is GotoChoice, guaranteeing a transition is selected.
      let msg = Message "forwarded message"
      pure $ case intent of
        IntentRefund    -> gotoChoice @"sgRefund" msg
        IntentQuestion  -> gotoChoice @"sgFaq" msg
        IntentComplaint -> gotoChoice @"sgFaq" msg  -- Complaints go to FAQ for now
  , sgRefund   = LLMBefore $ \_msg -> do
      -- Before handler: builds refund template context
      pure SimpleContext { scContent = "Processing refund..." }
  , sgFaq      = LLMBefore $ \_msg -> do
      -- Before handler: builds FAQ template context
      pure SimpleContext { scContent = "Here are some common questions..." }
  , sgExit     = Proxy @Response
  }
  where
    -- Helper to extract message content as Text
    msgContent :: Message -> Text
    msgContent (Message s) = T.pack s

-- | Test that ValidGraphRecord works on record-based graphs.
--
-- This uses the constraint to verify at compile time that:
-- * SupportGraph has an Entry field (sgEntry :: mode :- G.Entry Message)
-- * SupportGraph has an Exit field (sgExit :: mode :- G.Exit Response)
-- * All Goto targets reference valid field names
--
-- If any validation fails, you get a compile-time error.
validRecordGraph :: G.ValidGraphRecord SupportGraph => ()
validRecordGraph = ()

-- | ReifyRecordGraph instance for SupportGraph.
--
-- Enables automatic derivation of GraphInfo for Mermaid diagram generation:
--
-- @
-- import Tidepool.Graph.Mermaid (toMermaid)
--
-- diagram :: Text
-- diagram = toMermaid (reifyRecordGraph (Proxy \@SupportGraph))
-- @
--
-- Note: Due to polykind limitations, Goto targets are not extracted at runtime.
-- The diagram will show implicit edges (Entry/Schema → Needs) but not
-- explicit Goto transitions.
instance ReifyRecordGraph SupportGraph where
  reifyRecordGraph = makeGraphInfo
