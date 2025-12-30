{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example graph definitions and diagram generation.
--
-- This module contains example graphs that verify the DSL compiles correctly,
-- plus manually-constructed GraphInfo for diagram generation.
module Tidepool.Graph.Example
  ( -- * Example Graphs (type-level)
    SimpleGraph
  , BranchingGraph
  , AnnotatedGraph
  , ToolsGraph
  , MemoryGraph
  , TemplateGraph

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

    -- * Graph Info (runtime)
  , simpleGraphInfo
  , branchingGraphInfo
  , annotatedGraphInfo

    -- * Generated Diagrams
  , allDiagrams
  , printAllDiagrams
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))
import Effectful (type (:>))
import Effectful.State.Static.Local (State, get)
import GHC.Generics (Generic)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)
import Tidepool.Graph.Goto (Goto)
import Tidepool.Graph.Tool (ToolDef(..), toolToInfo)
import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)
import Tidepool.Graph.Example.Context (ClassifyContext(..))
import Tidepool.Schema (HasJSONSchema(..), JSONSchema(..), SchemaType(..), objectSchema, arraySchema, describeField, emptySchema)
import Tidepool.Graph.Reify
import Tidepool.Graph.Mermaid

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
-- SIMPLE LINEAR GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | A simple graph: Entry → classify → respond → Exit
--
-- @
-- ((start)) --> classify[[LLM]] --> respond[[LLM]] --> ((end))
-- @
type SimpleGraph = Graph
  '[ Entry :~> Message
   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
   , "respond"  := LLM :@ Needs '[Message, Intent] :@ Schema Response
   , Exit :<~ Response
   ]

-- | This constraint verifies SimpleGraph is valid at compile time.
-- If the graph were invalid, this would produce a type error.
validSimpleGraph :: ValidGraph SimpleGraph => ()
validSimpleGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- BRANCHING GRAPH WITH GOTO
-- ════════════════════════════════════════════════════════════════════════════

-- | A graph with branching logic
--
-- @
--                    ┌─→ refund[[LLM]] ──┐
-- ((start)) → route{{Logic}} ──────────────→ ((end))
--                    └─→ answer[[LLM]] ──┘
-- @
type BranchingGraph = Graph
  '[ Entry :~> Message
   , "route"  := Logic
       :@ Needs '[Message]
       :@ Eff '[Goto "refund" Message, Goto "answer" Message, Goto Exit Response]
   , "refund" := LLM :@ Needs '[Message] :@ Schema Response
   , "answer" := LLM :@ Needs '[Message] :@ Schema Response
   , Exit :<~ Response
   ]

validBranchingGraph :: ValidGraph BranchingGraph => ()
validBranchingGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH WITH ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

data MyTemplate
data PhotoTool

-- | A more complex graph with various annotations
type AnnotatedGraph = Graph
  '[ Entry :~> Message
   , "analyze" := LLM
       :@ Needs '[Message]
       :@ Schema Intent
       :@ Template MyTemplate
       :@ Vision
       :@ Tools '[PhotoTool]
   , "conditional" := LLM
       :@ Needs '[Intent]
       :@ Schema Response
       :@ When Intent  -- Only runs when Intent is present
   , Exit :<~ Response
   ]

validAnnotatedGraph :: ValidGraph AnnotatedGraph => ()
validAnnotatedGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH WITH V2 TOOLS
-- ════════════════════════════════════════════════════════════════════════════

-- | A graph using schema-driven tools.
--
-- The 'Tools' annotation references 'SearchTool' which has:
--
-- * 'ToolDef' instance with input/output types and effects
-- * 'HasJSONSchema' instances for input/output types
--
-- At compile time, 'ValidGraph' ensures all tools have proper schemas.
type ToolsGraph = Graph
  '[ Entry :~> Message
   , "search" := LLM
       :@ Needs '[Message]
       :@ Schema Response
       :@ Tools '[SearchTool]  -- V2 tool with schema validation
   , Exit :<~ Response
   ]

-- | This validates ToolsGraph at compile time, including tool schemas.
--
-- If SearchTool lacked proper HasJSONSchema instances, this would fail.
validToolsGraph :: ValidGraph ToolsGraph => ()
validToolsGraph = ()

-- | Demonstrate runtime reification of tool info.
--
-- This shows how to get tool metadata (names, descriptions, schemas) at runtime.
-- With the unified API, we use 'toolToInfo' on tool instances.
exampleToolReification :: [ToolInfo]
exampleToolReification = [toolToInfo SearchTool]

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH WITH MEMORY (Node-Private Persistent State)
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

-- | Intermediate type for research findings.
data Findings = Findings { findingsList :: [Text] }

-- | A graph demonstrating the Memory annotation for node-private state.
--
-- The "explore" node has its own ExploreMem that persists across runs.
-- This allows it to avoid repeating searches and build up knowledge.
--
-- The graph also demonstrates Global for shared state.
type MemoryGraph = Graph
  '[ Entry :~> Message
   , "explore" := LLM
       :@ Needs '[Message]
       :@ Schema Findings
       :@ Memory ExploreMem      -- Node-private persistent state
   , "summarize" := LLM
       :@ Needs '[Findings]
       :@ Schema Response
   , Exit :<~ Response
   ]
  :& Global SessionState         -- Graph-level shared state

-- | Validates MemoryGraph at compile time.
validMemoryGraph :: ValidGraph MemoryGraph => ()
validMemoryGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH WITH TEMPLATEDEF (Typed Prompt Templates)
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

-- | A graph using the TemplateDef system.
--
-- The @Template ClassifyTpl@ annotation tells the runner which template
-- to use for generating the LLM prompt for this node.
type TemplateGraph = Graph
  '[ Entry :~> Message
   , "classify" := LLM
       :@ Needs '[Message]
       :@ Template ClassifyTpl   -- Uses our typed template
       :@ Schema Intent
   , "respond" := LLM
       :@ Needs '[Message, Intent]
       :@ Schema Response
   , Exit :<~ Response
   ]

-- | Validates TemplateGraph at compile time.
validTemplateGraph :: ValidGraph TemplateGraph => ()
validTemplateGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- INVALID GRAPHS (commented out - uncomment to see compile errors)
-- ════════════════════════════════════════════════════════════════════════════

-- | Invalid: Goto references non-existent node "nonexistent"
--
-- Uncommenting produces:
--
-- @
-- Graph validation failed: invalid Goto target
-- Node 'badRoute' has:
--   Goto "nonexistent" ...
-- But no node named "nonexistent" exists.
-- Fix: Create the target node or use Goto Exit for termination.
-- @
--
-- type InvalidGotoGraph = Graph
--   '[ Entry :~> Message
--    , "badRoute" := Logic
--        :@ Needs '[Message]
--        :@ Eff '[Goto "nonexistent" Message]
--    , Exit :<~ Response
--    ]
--
-- invalidGotoTest :: ValidGraph InvalidGotoGraph => String
-- invalidGotoTest = "should not compile"
--
-- testInvalid :: String
-- testInvalid = invalidGotoTest

-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME GRAPH INFO (manually constructed for diagram generation)
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime info for SimpleGraph.
simpleGraphInfo :: GraphInfo
simpleGraphInfo = GraphInfo
  { giEntryType = Just (typeRep (Proxy @Message))
  , giExitType = Just (typeRep (Proxy @Response))
  , giNodes =
      [ NodeInfo
          { niName = "classify"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Message)]
          , niSchema = Just (typeRep (Proxy @Intent))
          , niGotoTargets = []
          , niHasGotoExit = False
          , niIsConditional = False
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      , NodeInfo
          { niName = "respond"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Message), typeRep (Proxy @Intent)]
          , niSchema = Just (typeRep (Proxy @Response))
          , niGotoTargets = []
          , niHasGotoExit = True  -- Last node exits
          , niIsConditional = False
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      ]
  , giEdges = []  -- Derived from Schema/Needs
  , giGroups = []
  }

-- | Runtime info for BranchingGraph.
branchingGraphInfo :: GraphInfo
branchingGraphInfo = GraphInfo
  { giEntryType = Just (typeRep (Proxy @Message))
  , giExitType = Just (typeRep (Proxy @Response))
  , giNodes =
      [ NodeInfo
          { niName = "route"
          , niKind = RuntimeLogic
          , niNeeds = [typeRep (Proxy @Message)]
          , niSchema = Nothing
          , niGotoTargets =
              [ ("refund", typeRep (Proxy @Message))
              , ("answer", typeRep (Proxy @Message))
              ]
          , niHasGotoExit = True  -- Can also exit directly
          , niIsConditional = False
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      , NodeInfo
          { niName = "refund"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Message)]
          , niSchema = Just (typeRep (Proxy @Response))
          , niGotoTargets = []
          , niHasGotoExit = True
          , niIsConditional = False
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      , NodeInfo
          { niName = "answer"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Message)]
          , niSchema = Just (typeRep (Proxy @Response))
          , niGotoTargets = []
          , niHasGotoExit = True
          , niIsConditional = False
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      ]
  , giEdges = []
  , giGroups = []
  }

-- | Runtime info for AnnotatedGraph.
annotatedGraphInfo :: GraphInfo
annotatedGraphInfo = GraphInfo
  { giEntryType = Just (typeRep (Proxy @Message))
  , giExitType = Just (typeRep (Proxy @Response))
  , giNodes =
      [ NodeInfo
          { niName = "analyze"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Message)]
          , niSchema = Just (typeRep (Proxy @Intent))
          , niGotoTargets = []
          , niHasGotoExit = False
          , niIsConditional = False
          , niHasVision = True
          , niTools = [typeRep (Proxy @PhotoTool)]
          , niToolInfos = []  -- Would be populated by reification
          , niTemplate = Just (typeRep (Proxy @MyTemplate))
          , niMemory = Nothing
          }
      , NodeInfo
          { niName = "conditional"
          , niKind = RuntimeLLM
          , niNeeds = [typeRep (Proxy @Intent)]
          , niSchema = Just (typeRep (Proxy @Response))
          , niGotoTargets = []
          , niHasGotoExit = True
          , niIsConditional = True  -- Has When annotation
          , niHasVision = False
          , niTools = []
          , niToolInfos = []
          , niTemplate = Nothing
          , niMemory = Nothing
          }
      ]
  , giEdges = []
  , giGroups = []
  }

-- ════════════════════════════════════════════════════════════════════════════
-- DIAGRAM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | All diagrams as (name, diagram) pairs.
allDiagrams :: [(Text, Text)]
allDiagrams =
  [ -- SimpleGraph
    ("SimpleGraph - Flowchart", toMermaid simpleGraphInfo)
  , ("SimpleGraph - State Diagram", toStateDiagram simpleGraphInfo)
  , ("SimpleGraph - Sequence Diagram",
      toSequenceDiagram defaultConfig simpleGraphInfo
        (ExecutionPath ["classify", "respond"]))

    -- BranchingGraph
  , ("BranchingGraph - Flowchart", toMermaid branchingGraphInfo)
  , ("BranchingGraph - State Diagram", toStateDiagram branchingGraphInfo)
  , ("BranchingGraph - Sequence (refund path)",
      toSequenceDiagram defaultConfig branchingGraphInfo
        (ExecutionPath ["route", "refund"]))
  , ("BranchingGraph - Sequence (answer path)",
      toSequenceDiagram defaultConfig branchingGraphInfo
        (ExecutionPath ["route", "answer"]))

    -- AnnotatedGraph
  , ("AnnotatedGraph - Flowchart", toMermaid annotatedGraphInfo)
  , ("AnnotatedGraph - State Diagram", toStateDiagram annotatedGraphInfo)
  ]

-- | Print all diagrams to stdout.
printAllDiagrams :: IO ()
printAllDiagrams = mapM_ printDiagram allDiagrams
  where
    printDiagram (name, diagram) = do
      TIO.putStrLn $ "\n" <> name
      TIO.putStrLn $ T.replicate (T.length name) "="
      TIO.putStrLn diagram
