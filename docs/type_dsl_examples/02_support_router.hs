{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Customer Support Router
--
-- Classify intent, route to specialist handlers.
-- Demonstrates: multi-way branching via multiple Goto effects.
--
-- ```mermaid
-- flowchart TD
--     entry((start)) -->|Message| classify
--     classify[["classify<br/>LLM"]] -->|Intent| route
--     route{{"route<br/>Logic"}} -->|Message| refund
--     route -->|Message| technical
--     route -->|Message| billing
--     route -->|Message| general
--     refund[["refund<br/>LLM"]] -->|Response| formatResponse
--     technical[["technical<br/>LLM"]] -->|Response| formatResponse
--     billing[["billing<br/>LLM"]] -->|Response| formatResponse
--     general[["general<br/>LLM"]] -->|Response| formatResponse
--     formatResponse{{"formatResponse<br/>Logic"}} -->|Text| exit((end))
-- ```

module Examples.SupportRouter where

import Tidepool.Graph

-- ════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════

data Message = Message
  { msgFrom :: Text
  , msgBody :: Text
  , msgHistory :: [Text]
  }

data Intent = Refund | Technical | Billing | General
  deriving (Eq, Show, Generic, FromJSON)

data Response = Response
  { respBody :: Text
  , respSuggestedActions :: [Text]
  }
  deriving (Generic, ToJSON)

-- ════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════

type SupportGraph = Graph '[
    Entry :~> Message

    -- Classify intent (LLM node)
  , "classify" := LLM
      :@ Needs '[Message]
      :@ Template ClassifyTpl
      :@ Schema Intent

    -- Route based on intent (Logic node with multiple Goto options)
  , "route" := Logic
      :@ Needs '[Message, Intent]
      :@ Eff '[
          Goto "refund" Message
        , Goto "technical" Message
        , Goto "billing" Message
        , Goto "general" Message
        ]

    -- Specialist handlers (all LLM nodes)
  , "refund" := LLM
      :@ Needs '[Message]
      :@ Template RefundTpl
      :@ Schema Response

  , "technical" := LLM
      :@ Needs '[Message]
      :@ Template TechnicalTpl
      :@ Schema Response

  , "billing" := LLM
      :@ Needs '[Message]
      :@ Template BillingTpl
      :@ Schema Response

  , "general" := LLM
      :@ Needs '[Message]
      :@ Template GeneralTpl
      :@ Schema Response

    -- Format response for output
  , "formatResponse" := Logic
      :@ Needs '[Response]
      :@ Eff '[Goto Exit Text]

  , Exit :<~ Text
  ]
    :& Groups '[
        "intake"   := '["classify", "route"]
      , "handlers" := '["refund", "technical", "billing", "general"]
      ]

-- Edge derivation:
--   Entry → classify (classify Needs Message)
--   Entry → route (route Needs Message)
--   classify → route (route Needs Intent, classify Schema Intent)
--   route → refund (route has Goto "refund" Message)
--   route → technical (route has Goto "technical" Message)
--   route → billing (route has Goto "billing" Message)
--   route → general (route has Goto "general" Message)
--   refund → formatResponse (formatResponse Needs Response)
--   technical → formatResponse (formatResponse Needs Response)
--   billing → formatResponse (formatResponse Needs Response)
--   general → formatResponse (formatResponse Needs Response)
--   formatResponse → Exit (formatResponse has Goto Exit Text)

-- ════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════

-- Handler types:
--   classify :: Message -> LLM Intent
--   route :: Message -> Intent -> Eff '[Goto "refund" Message, ...] ()
--   refund :: Message -> LLM Response
--   formatResponse :: Response -> Eff '[Goto Exit Text] ()

supportHandlers :: Handlers SupportGraph
supportHandlers = Handlers
  { classify = \msg ->
      runLLM classifyTemplate msg.msgBody

  , route = \msg intent ->
      -- Use the appropriate Goto based on intent
      case intent of
        Refund    -> goto @"refund" msg
        Technical -> goto @"technical" msg
        Billing   -> goto @"billing" msg
        General   -> goto @"general" msg

  , refund    = \msg -> runLLM refundTemplate msg
  , technical = \msg -> runLLM technicalTemplate msg
  , billing   = \msg -> runLLM billingTemplate msg
  , general   = \msg -> runLLM generalTemplate msg

  , formatResponse = \resp ->
      goto @Exit (resp.respBody)
  }

-- ════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════

-- This example demonstrates:
--
-- 1. Multi-way branching via Goto effects
--    - route has 4 Goto effects in its Eff stack
--    - Each Goto is a capability the handler can use
--    - Handler calls exactly one goto based on runtime decision
--
-- 2. No sum types needed
--    - Old approach: Routes generated a sum type, handler returned it
--    - New approach: Handler calls goto directly, no intermediate type
--
-- 3. Fan-in at formatResponse
--    - All 4 specialist handlers produce Response (via Schema)
--    - formatResponse Needs Response
--    - Whichever handler runs provides the Response
--
-- 4. Groups for Mermaid organization
--    - "intake" group: classify + route
--    - "handlers" group: the 4 specialists
--    - Renders as subgraph boxes in Mermaid
