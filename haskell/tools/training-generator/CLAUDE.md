# Training Generator - FunctionGemma Fine-Tuning Data

Types and utilities for generating training data to fine-tune FunctionGemma 270M as a semantic code scorer. Used by semantic-scout to replace heuristics with learned models.

## When to Read This

Read this if you're:
- Working on semantic-scout training data generation
- Understanding the heuristics → ML migration path
- Generating datasets for FunctionGemma fine-tuning
- Defining new Tag types for code exploration
- Working with semantic analysis rubrics

## What It Does

Defines shared types for:
1. **Training examples**: (query, node, rubric) triples for supervised learning
2. **Tag types**: Categories of breaking changes (Exhaustive, PatternMatch, etc.)
3. **Rubrics**: Structured scores (relevance, risk, confidence, reasoning)
4. **Node context**: Code location + hover + snippet

These types are used by:
- **semantic-scout**: Collects training examples during exploration
- **Training pipeline**: Converts examples to FunctionGemma format
- **Heuristics**: Pattern-based scoring (to be replaced by ML)

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ semantic-scout --gen-training 100                                   │
│   • Explores codebase with heuristic scorer                         │
│   • Collects (query, node, rubric) triples                          │
│   • Exports as JSON                                                 │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ JSON
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Training Pipeline (external)                                        │
│   • Converts JSON → FunctionGemma format                            │
│   • Splits train/val/test sets                                      │
│   • Fine-tunes FunctionGemma 270M                                   │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Model
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ semantic-scout --mcp                                                │
│   • Uses trained model instead of heuristics                        │
│   • HTTP to FunctionGemma inference server                          │
└─────────────────────────────────────────────────────────────────────┘
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Types.hs` | Core data types (TrainingExample, Tag, Rubric, etc.) |

## Data Model

### TrainingExample
```haskell
data TrainingExample = TrainingExample
  { teQuery     :: QueryContext
  , teNode      :: NodeContext
  , teRubric    :: Rubric
  , teTimestamp :: UTCTime
  } deriving (Generic, ToJSON, FromJSON)
```

**Purpose:** Single training instance for supervised learning.

**Fields:**
- `teQuery`: User's natural language query + tags
- `teNode`: Code location + hover info + snippet
- `teRubric`: Expected output (scores + reasoning)
- `teTimestamp`: When example was collected

### QueryContext
```haskell
data QueryContext = QueryContext
  { qcQuery :: Text       -- Natural language question
  , qcTags  :: [Tag]      -- What kind of changes break code
  } deriving (Generic, ToJSON, FromJSON)
```

**Example:**
```json
{
  "query": "What breaks if I add a variant to LLMKind?",
  "tags": ["Exhaustive", "PatternMatch"]
}
```

### NodeContext
```haskell
data NodeContext = NodeContext
  { ncLocation :: Text           -- "File.hs:45"
  , ncHover    :: Maybe Text     -- Hover info from LSP
  , ncSnippet  :: Text           -- Code snippet (5 lines)
  } deriving (Generic, ToJSON, FromJSON)
```

**Example:**
```json
{
  "location": "Types.hs:127",
  "hover": "data LLMKind = Sonnet | Opus | Haiku",
  "snippet": "data LLMKind\n  = Sonnet\n  | Opus\n  | Haiku\n  deriving (Show, Eq)"
}
```

### Rubric
```haskell
data Rubric = Rubric
  { rRelevance  :: Int   -- 1-5: How relevant to query
  , rRisk       :: Int   -- 1-5: Impact of breaking change
  , rConfidence :: Int   -- 1-5: How confident in scores
  , rReasoning  :: Text  -- Explanation
  } deriving (Generic, ToJSON, FromJSON)
```

**Example:**
```json
{
  "relevance": 5,
  "risk": 5,
  "confidence": 4,
  "reasoning": "Exhaustive pattern match on LLMKind - must add case for new variant"
}
```

**Scoring guide:**

| Score | Relevance | Risk |
|-------|-----------|------|
| 5 | Critical to query | Breaking change required |
| 4 | Highly relevant | Major refactoring needed |
| 3 | Moderately relevant | Minor changes needed |
| 2 | Tangentially related | Might need update |
| 1 | Barely relevant | No changes needed |

### Tag
```haskell
data Tag
  = Exhaustive      -- Exhaustive pattern matches (every constructor)
  | PatternMatch    -- Specific pattern matches
  | TypeFamily      -- Type family definitions
  | Constraint      -- Typeclass constraints
  | DataType        -- Data type definitions
  | TypeAlias       -- Type aliases
  | Function        -- Function definitions
  | Module          -- Module exports
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Purpose:** Signal what kind of code changes break existing code.

**Usage:**
```haskell
-- User query
query = ScoutQuery
  { query = "What breaks if I add a variant to LLMKind?"
  , tags = [Exhaustive, PatternMatch]  -- Look for these patterns
  , budget = 20
  }
```

**Heuristic matching:**
```haskell
scoreNode :: QueryContext -> NodeContext -> Rubric
scoreNode query node
  | "pattern" `isInfixOf` hover && Exhaustive `elem` qcTags query =
      Rubric 5 5 4 "Exhaustive pattern match"
  | "data" `isInfixOf` hover && DataType `elem` qcTags query =
      Rubric 4 3 5 "Data type definition"
  | ...
```

## Training Data Generation

### Flow

1. **Run semantic-scout in training mode:**
   ```bash
   semantic-scout --gen-training 100 > training.json
   ```

2. **Exploration with heuristics:**
   - For each query, explore codebase
   - Heuristic scorer assigns rubrics
   - Collect (query, node, rubric) triples

3. **Export JSON:**
   ```json
   [
     {
       "query": {
         "query": "What breaks if I add a variant to LLMKind?",
         "tags": ["Exhaustive", "PatternMatch"]
       },
       "node": {
         "location": "Types.hs:127",
         "hover": "data LLMKind = ...",
         "snippet": "data LLMKind\n  = Sonnet\n  ..."
       },
       "rubric": {
         "relevance": 5,
         "risk": 5,
         "confidence": 4,
         "reasoning": "Exhaustive pattern match"
       },
       "timestamp": "2026-01-15T10:30:00Z"
     },
     ...
   ]
   ```

4. **Training pipeline (external):**
   - Converts to FunctionGemma format
   - Splits train/val/test
   - Fine-tunes model

5. **Deploy trained model:**
   - Replace `runGemmaHeuristic` with `runGemmaModel`
   - HTTP to inference server

### Dataset Guidelines

**Volume:**
- Minimum: 1000 examples
- Target: 10,000+ examples
- Per-tag distribution: balanced across tags

**Quality:**
- Diverse queries (not just "what breaks")
- Multiple codebases (not just Tidepool)
- Multiple languages (if supporting non-Haskell)

**Validation:**
- Hold out 20% for validation
- Test on unseen codebases

## FunctionGemma Integration

**Model:** FunctionGemma 270M (Google)

**Input format:**
```json
{
  "query": "What breaks if I add a variant to LLMKind?",
  "tags": ["Exhaustive", "PatternMatch"],
  "location": "Types.hs:127",
  "hover": "data LLMKind = Sonnet | Opus | Haiku",
  "snippet": "data LLMKind\n  = Sonnet\n  | Opus\n  | Haiku"
}
```

**Output format:**
```json
{
  "relevance": 5,
  "risk": 5,
  "confidence": 4,
  "reasoning": "Exhaustive pattern match on LLMKind - must add case for new variant"
}
```

**Deployment:**
- HTTP endpoint: `POST /score`
- Latency target: <100ms per node
- Batch scoring: up to 50 nodes per request

## Heuristics → ML Migration Path

### Phase 1: Heuristics (Current)
```haskell
runGemmaHeuristic :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaHeuristic = interpret $ \case
  RateNode query node -> pure $ scoreNode query node
```

Pattern-based rules in `Heuristics.hs`.

### Phase 2: Data Collection
```bash
semantic-scout --gen-training 10000 > dataset.json
```

Run on multiple codebases, collect diverse examples.

### Phase 3: Training
External pipeline:
1. Convert to FunctionGemma format
2. Fine-tune 270M model
3. Evaluate on held-out test set

### Phase 4: Deployment
```haskell
runGemmaModel :: String -> Eff (Gemma ': effs) a -> Eff effs a
runGemmaModel url = interpret $ \case
  RateNode query node -> do
    -- HTTP POST to inference server
    liftIO $ httpPost url (encode (query, node))
```

### Phase 5: Hybrid Mode
```haskell
runGemmaHybrid :: String -> Eff (Gemma ': effs) a -> Eff effs a
runGemmaHybrid url = interpret $ \case
  RateNode query node -> do
    -- Try model first
    modelResult <- liftIO $ tryHttpPost url (encode (query, node))
    case modelResult of
      Just rubric -> pure rubric
      Nothing -> pure $ scoreNode query node  -- Fallback to heuristics
```

## Usage in semantic-scout

```haskell
-- Import training types
import Tidepool.TrainingGenerator.Types

-- Collect during exploration
exploreEff :: ScoutQuery -> Eff effs ScoutResponse
exploreEff query = do
  examples <- foldM collectExample [] nodes
  pure $ ScoutResponse
    { summary = ...
    , pointers = ...
    , trainingExamples = examples  -- Export for training
    }

collectExample :: [TrainingExample] -> Node -> Eff effs [TrainingExample]
collectExample acc node = do
  rubric <- rateNode (toQueryContext query) (toNodeContext node)
  now <- liftIO getCurrentTime
  pure $ TrainingExample
    { teQuery = toQueryContext query
    , teNode = toNodeContext node
    , teRubric = rubric
    , teTimestamp = now
    } : acc
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Separate package | Shared types between scout and training pipeline |
| Tag-based queries | Guides exploration, improves model accuracy |
| Rubric structure | Interpretable scores + reasoning for debugging |
| JSON format | Standard, tooling-friendly, language-agnostic |
| Timestamp | Track data freshness, enable temporal analysis |
| Heuristics first | Bootstraps dataset without ML infrastructure |

## Related Documentation

- [agents/semantic-scout/CLAUDE.md](../../agents/semantic-scout/CLAUDE.md) - Uses these types
- [effects/lsp-interpreter/CLAUDE.md](../../effects/lsp-interpreter/CLAUDE.md) - LSP for node context
- [tools/CLAUDE.md](../CLAUDE.md) - Tools overview
- [Root CLAUDE.md](../../../CLAUDE.md) - Project overview

## Future Work

- Auto-generate training examples from git history (what actually broke)
- Active learning: prioritize uncertain examples for human review
- Multi-language support (Go, Rust, TypeScript)
- Rubric expansion: add "changeComplexity", "testCoverage" scores
- Continuous training: retrain model periodically with new examples
