{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Parallel Analysis with Fan-In
--
-- Analyze document from multiple angles, synthesize results.
-- Demonstrates: fan-out via Needs, fan-in via multiple Goto to same target, Groups.
--
-- ```mermaid
-- flowchart TD
--     entry((start)) -->|Document| sentiment
--     entry -->|Document| topics
--     entry -->|Document| entities
--
--     subgraph analysis
--         sentiment{{"sentiment<br/>Logic"}}
--         topics{{"topics<br/>Logic"}}
--         entities{{"entities<br/>Logic"}}
--     end
--
--     sentiment -->|Sentiment| synthesize
--     topics -->|Topics| synthesize
--     entities -->|Entities| synthesize
--
--     synthesize[["synthesize<br/>LLM"]] -->|Summary| format
--     format{{"format<br/>Logic"}} -->|Text| exit((end))
-- ```

module Examples.ParallelAnalysis where

import Tidepool.Graph

-- ════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════

data Document = Document
  { docTitle :: Text
  , docContent :: Text
  , docMetadata :: Map Text Text
  }

data Sentiment = Sentiment
  { sentScore :: Float
  , sentLabel :: Text
  , sentConfidence :: Float
  }
  deriving (Generic, FromJSON)

data Topic = Topic
  { topicName :: Text
  , topicRelevance :: Float
  , topicKeyPhrases :: [Text]
  }
  deriving (Generic, FromJSON)

data Entity = Entity
  { entityName :: Text
  , entityType :: Text
  , entityMentions :: Int
  }
  deriving (Generic, FromJSON)

data Summary = Summary
  { sumOverview :: Text
  , sumSentiment :: Text
  , sumMainTopics :: [Text]
  , sumKeyEntities :: [Text]
  , sumRecommendations :: [Text]
  }
  deriving (Generic, ToJSON)

-- ════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════

type AnalysisGraph = Graph '[
    Entry :~> Document

    -- Fan-out: all three analyzers need Document
    -- They can run in parallel
  , "sentiment" := Logic
      :@ Needs '[Document]
      :@ Eff '[LLM, Goto "synthesize" Sentiment]

  , "topics" := Logic
      :@ Needs '[Document]
      :@ Eff '[LLM, Goto "synthesize" [Topic]]

  , "entities" := Logic
      :@ Needs '[Document]
      :@ Eff '[LLM, Goto "synthesize" [Entity]]

    -- Fan-in: synthesize needs ALL three outputs
    -- Runner waits for all Goto's before calling
  , "synthesize" := LLM
      :@ Needs '[Document, Sentiment, [Topic], [Entity]]
      :@ Template SynthesisTpl
      :@ Schema Summary

    -- Format for output
  , "format" := Logic
      :@ Needs '[Summary]
      :@ Eff '[Goto Exit Text]

  , Exit :<~ Text
  ]
    :& Groups '[
        "analysis" := '["sentiment", "topics", "entities"]
      , "output"   := '["synthesize", "format"]
      ]
    :& Requires '[LLM]

-- Edge derivation:
--   Entry → sentiment (sentiment Needs Document)
--   Entry → topics (topics Needs Document)
--   Entry → entities (entities Needs Document)
--   Entry → synthesize (synthesize Needs Document)
--   sentiment → synthesize (sentiment Goto "synthesize" Sentiment)
--   topics → synthesize (topics Goto "synthesize" [Topic])
--   entities → synthesize (entities Goto "synthesize" [Entity])
--   synthesize → format (format Needs Summary, synthesize Schema Summary)
--   format → Exit (format Goto Exit Text)

-- ════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════

-- Handler types:
--   sentiment :: Document -> Eff '[LLM, Goto "synthesize" Sentiment] ()
--   topics :: Document -> Eff '[LLM, Goto "synthesize" [Topic]] ()
--   entities :: Document -> Eff '[LLM, Goto "synthesize" [Entity]] ()
--   synthesize :: Document -> Sentiment -> [Topic] -> [Entity] -> LLM Summary
--   format :: Summary -> Eff '[Goto Exit Text] ()

analysisHandlers :: Handlers AnalysisGraph
analysisHandlers = Handlers
  { sentiment = \doc -> do
      result <- runLLM sentimentTemplate doc.docContent
      goto @"synthesize" result

  , topics = \doc -> do
      result <- runLLM topicsTemplate doc.docContent
      goto @"synthesize" result

  , entities = \doc -> do
      result <- runLLM entitiesTemplate doc.docContent
      goto @"synthesize" result

  , synthesize = \doc sent topics ents ->
      runLLM synthesisTemplate (doc, sent, topics, ents)

  , format = \summary ->
      goto @Exit (formatSummary summary)
  }

-- ════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════

-- This example demonstrates:
--
-- 1. Fan-out (parallel execution)
--    - Three Logic nodes all Needs '[Document]
--    - Runner can execute all three in parallel
--    - No explicit fork/parallel declaration needed
--
-- 2. Fan-in via multiple Goto to same target
--    - sentiment Goto "synthesize" Sentiment
--    - topics Goto "synthesize" [Topic]
--    - entities Goto "synthesize" [Entity]
--    - Each provides one of synthesize's Needs
--
-- 3. Join semantics
--    - synthesize Needs '[Document, Sentiment, [Topic], [Entity]]
--    - Runner waits for ALL four before calling handler
--    - Document from Entry (implicit)
--    - Other three from Goto's (explicit)
--
-- 4. Logic nodes with LLM effect
--    - The analyzers use LLM effect to call the model
--    - But they're Logic nodes because they also Goto
--    - Pattern: do LLM work, then Goto with result
--
-- 5. Groups annotation
--    - "analysis" group: the three parallel analyzers
--    - "output" group: synthesize + format
--    - Renders as subgraph boxes in Mermaid
--
-- 6. Handler receives all inputs directly
--    - synthesize :: Document -> Sentiment -> [Topic] -> [Entity] -> LLM Summary
--    - Each Needs type becomes a parameter
--    - No intermediate accumulator or fromJust
