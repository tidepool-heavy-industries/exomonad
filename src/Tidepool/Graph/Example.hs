{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Example graph definitions and diagram generation.
--
-- This module contains example graphs that verify the DSL compiles correctly,
-- plus manually-constructed GraphInfo for diagram generation.
module Tidepool.Graph.Example
  ( -- * Example Graphs (type-level)
    SimpleGraph
  , BranchingGraph
  , AnnotatedGraph

    -- * Graph Info (runtime)
  , simpleGraphInfo
  , branchingGraphInfo
  , annotatedGraphInfo

    -- * Generated Diagrams
  , allDiagrams
  , printAllDiagrams
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable (typeRep)
import Data.Proxy (Proxy(..))

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)
import Tidepool.Graph.Goto (Goto)
import Tidepool.Graph.Reify
import Tidepool.Graph.Mermaid

-- ════════════════════════════════════════════════════════════════════════════
-- EXAMPLE TYPES
-- ════════════════════════════════════════════════════════════════════════════

data Message = Message { msgContent :: String }
data Intent = IntentRefund | IntentQuestion | IntentComplaint
data Response = Response { respText :: String }

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
          , niTemplate = Nothing
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
          , niTemplate = Nothing
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
          , niTemplate = Nothing
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
          , niTemplate = Nothing
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
          , niTemplate = Nothing
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
          , niTemplate = Just (typeRep (Proxy @MyTemplate))
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
          , niTemplate = Nothing
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
