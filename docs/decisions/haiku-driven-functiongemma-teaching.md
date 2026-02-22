# Haiku-Driven FunctionGemma Teaching

## Status
Accepted

## Context

We need to fine-tune FunctionGemma 270M for semantic code analysis tasks (symbol selection, edge scoring, dependency extraction). The traditional approaches have critical weaknesses:

1. **Synthetic generation** (QuickCheck + heuristics) produces brittle, unrealistic examples
2. **Manual annotation** doesn't scale and misses nuanced reasoning
3. **Isolated examples** lack the contextual richness of real exploration workflows

Meanwhile, we already have working production code:
- LSP integration provides rich, real code context
- Effect-based architecture makes interpreter swapping trivial
- Exploration graphs capture multi-turn reasoning workflows
- Tool definitions already exist for production use

## Decision

**Generate FunctionGemma training data by running actual production workflows with Haiku as the execution backend, recording its reasoning and tool use.**

### Core Principles

1. **One Tool, Two Backends**
   - Define tools once (schema + semantics)
   - Production: FunctionGemma via Ollama (fast, local, 270M params)
   - Teaching: Haiku via Anthropic API (smart, recorded, 200B params)
   - Same prompts, same tasks, different interpreters

2. **Task-Oriented, Not Roleplay**
   - Don't tell Haiku to "pretend to be FunctionGemma"
   - Give Haiku the actual task (select symbols, score edges, etc.)
   - Haiku performs real work; we record it

3. **Reasoning as First-Class Data**
   - Haiku thinks in natural language (its native mode)
   - Capture reasoning text alongside tool calls
   - Convert reasoning to inline comments in training data
   - FunctionGemma learns BOTH what to output AND why

4. **Real Context, Real Workflows**
   - Run actual LSP exploration graphs
   - Use real hover info, signatures, file locations
   - Multi-turn conversations from BFS exploration
   - Training data reflects production use cases

5. **Iteration Decoupled**
   - Record raw Anthropic responses (once, expensive ~$0.50/session)
   - Convert to FunctionGemma format offline (many times, free)
   - Tune output format without re-running Haiku
   - Preserve all information for future format changes

### Generic Machinery

Implement via `FineTrainingTeacher` typeclass:
- Task-specific guidance (appended to system prompt)
- Tool schemas (Anthropic Messages API format)
- Response parsing (typed results)
- Format conversion (Anthropic → FunctionGemma JSONL)

This enables teaching ANY fine-tuning task using the same infrastructure.

## Consequences

### Positive

- **Quality**: Training data comes from Haiku solving real problems on real code
- **Scale**: Automated recording means thousands of examples overnight
- **Variety**: Different exploration queries produce diverse training scenarios
- **Cost-effective**: ~$1.50 per 1000 examples (Haiku is cheap)
- **Maintainable**: One tool definition, two backends (production + teaching)
- **Iterative**: Tune FunctionGemma format without re-running expensive Haiku calls
- **Reusable**: Same machinery for all fine-tuning tasks (scoring, selection, etc.)

### Negative

- **API dependency**: Requires Anthropic API access during recording phase
- **Cold start**: Need initial Haiku run before FunctionGemma can replace it
- **Format coupling**: Conversion logic must handle both Anthropic and FunctionGemma formats
- **Quality variance**: Haiku's output quality depends on prompt engineering

### Neutral

- **Two-phase workflow**: Record (expensive, rare) → Convert (cheap, frequent)
- **Effect interpreter layer**: Requires understanding freer-simple and interpreter pattern
- **Post-processing**: Reasoning extraction and comment formatting needs implementation

## Implementation Strategy

1. Define `FineTrainingTeacher` typeclass for generic teaching
2. Create `runHaikuTeacherRecorder` interpreter (swaps for production interpreter)
3. Implement Anthropic Messages API integration with tool use
4. Build offline converter: raw recordings → FunctionGemma JSONL
5. Create task instances: `SelectSymbols`, `ScoreEdge`, etc.

## Examples

### Production Mode
```haskell
runM $ runTeachGemmaHTTP ollamaEndpoint $ runLSP session $
  teach config query
-- → FunctionGemma 270M via Ollama
```

### Teaching Mode
```haskell
runM $ runHaikuTeacherRecorder anthropicCfg outputFile $ runLSP session $
  teach config query
-- → Haiku via Anthropic, records conversations
```

Same graph. Same prompts. Different interpreter. That's it.

## Alternatives Considered

### Synthetic QuickCheck Generation
**Rejected**: Brittle, misses nuance, doesn't reflect real code patterns

### Manual Annotation
**Rejected**: Doesn't scale, loses reasoning context, tedious

### Scraping Public Repositories
**Rejected**: No ground truth, wrong domain (our codebase has specific patterns), legal concerns

### GPT-4 Instead of Haiku
**Rejected**: More expensive (~4x cost), slower, less control over prompting

## References

- Knowledge Distillation in Neural Networks (Hinton et al.)
- Constitutional AI (Anthropic)
- Behavioral Cloning for LLMs
- Effect Systems and Interpreter Pattern (freer-simple)
