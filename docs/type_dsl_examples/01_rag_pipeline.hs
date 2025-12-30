{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | RAG Pipeline
--
-- Simple retrieve-augment-generate pattern.
-- Demonstrates: linear flow, Goto effects, Needs-based implicit edges.
--
-- ```mermaid
-- flowchart TD
--     entry((start)) -->|Query| embed
--     embed[[\"embed<br/>LLM\"]] -->|Embedding| retrieve
--     retrieve{{\"retrieve<br/>Logic\"}} -->|Documents| generate
--     entry -->|Query| generate
--     generate[[\"generate<br/>LLM\"]] -->|Answer| format
--     format{{\"format<br/>Logic\"}} -->|Text| exit((end))
-- ```

module Examples.RAGPipeline where

import Tidepool.Graph

-- ════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════

newtype Query = Query Text
newtype Embedding = Embedding [Float]

data Document = Document
  { docId :: Text
  , docContent :: Text
  , docScore :: Float
  }

newtype Answer = Answer Text

-- ════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════

type RAGGraph = Graph '[
    Entry :~> Query

    -- Embed the query (LLM node - implicit output via Schema)
  , "embed" := LLM
      :@ Needs '[Query]
      :@ Template EmbedTpl
      :@ Schema Embedding

    -- Retrieve from vector DB (Logic node - explicit Goto)
  , "retrieve" := Logic
      :@ Needs '[Embedding]
      :@ Eff '[VectorDB, Log, Goto "generate" [Document]]

    -- Generate answer (LLM node - receives from Entry and retrieve)
  , "generate" := LLM
      :@ Needs '[Query, [Document]]
      :@ Template GenerateTpl
      :@ Schema Answer

    -- Format output (Logic node - pure, just Goto)
  , "format" := Logic
      :@ Needs '[Answer]
      :@ Eff '[Goto Exit Text]

  , Exit :<~ Text
  ]
    :& Requires '[LLM, VectorDB, Log]

-- Edge derivation:
--   Entry → embed (embed Needs Query, Entry provides Query)
--   embed → retrieve (retrieve Needs Embedding, embed Schema Embedding)
--   retrieve → generate (retrieve Goto "generate" [Document])
--   Entry → generate (generate Needs Query)
--   generate → format (format Needs Answer, generate Schema Answer)
--   format → Exit (format Goto Exit Text)

-- ════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════

-- Handler types:
--   embed :: Query -> LLM Embedding
--   retrieve :: Embedding -> Eff '[VectorDB, Log, Goto "generate" [Document]] ()
--   generate :: Query -> [Document] -> LLM Answer
--   format :: Answer -> Eff '[Goto Exit Text] ()

ragHandlers :: Handlers RAGGraph
ragHandlers = Handlers
  { embed = \(Query q) ->
      runLLM embedTemplate q

  , retrieve = \emb -> do
      docs <- queryVectorDB emb 10
      log $ "Retrieved " <> show (length docs) <> " documents"
      let ranked = take 5 $ sortOn (negate . docScore) docs
      goto @"generate" ranked

  , generate = \query docs ->
      runLLM generateTemplate (query, docs)

  , format = \(Answer a) ->
      goto @Exit ("## Answer\n\n" <> a)
  }

-- ════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════

-- This example demonstrates:
--
-- 1. Two types of edge derivation
--    - Implicit: LLM Schema → whoever Needs it (embed → retrieve)
--    - Explicit: Logic Goto → named target (retrieve → generate)
--
-- 2. Logic nodes use Goto for output
--    - retrieve does: goto @"generate" docs
--    - format does: goto @Exit text
--    - No "return value" - output is via the effect
--
-- 3. LLM nodes use Schema for output
--    - embed produces Embedding (flows to retrieve via Needs)
--    - generate produces Answer (flows to format via Needs)
--
-- 4. Fan-in at generate
--    - generate Needs '[Query, [Document]]
--    - Query comes from Entry (implicit)
--    - [Document] comes from retrieve's Goto (explicit)
--    - Runner waits for both before calling generate
