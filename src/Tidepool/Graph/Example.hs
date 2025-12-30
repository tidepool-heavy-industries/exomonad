{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Example graph definition to verify the DSL compiles correctly.
module Tidepool.Graph.Example where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)
import Tidepool.Graph.Goto (Goto)

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
