# FunctionGemma Training Data Annotation Instructions

You are annotating training data for FunctionGemma, a small (270M parameter) function-calling model. Your task is to fill in annotation holes in JSONL training examples.

## Task Overview

Each line in `functiongemma-holes.jsonl` is a training example with two holes to fill:
1. `<!-- REPLACE: topic -->` - A semantic query describing what someone might ask about this symbol
2. `<!-- REPLACE: selected -->` - Comma-separated list of relevant candidate symbols (or empty)

## Format

Each example has this structure in the `text` field:

```
<start_of_turn>user
Topic: <escape><!-- REPLACE: topic --><escape>
Symbol: <escape>compositeScore<escape>
Location: <escape>/path/to/Scoring.hs:42<escape>
Signature: <escape>compositeScore :: ScoreConfig -> EdgeContext -> Double<escape>
Candidates: ScoreConfig, EdgeContext, Rubric
<end_of_turn>
<start_of_turn>model
<start_function_call>
call:select_symbols{selected:<escape><!-- REPLACE: selected --><escape>}
<end_function_call>
<end_of_turn>
```

## Filling the `topic` Hole

Write a natural query someone might ask when exploring this symbol. Examples:

- "How does the scoring system evaluate edges?"
- "What configuration options affect score calculation?"
- "Understanding the context passed to scoring functions"

**Guidelines:**
- Be specific to the symbol's domain/purpose
- Write as if you're a developer trying to understand the code
- Keep it concise (one sentence)

## Filling the `selected` Hole

Select which candidates are **semantically relevant** to understanding the symbol in context of the topic.

**Format:** Comma-separated names, no spaces after commas, no quotes.

Examples:
- All relevant: `ScoreConfig,EdgeContext,Rubric`
- Some relevant: `ScoreConfig,EdgeContext`
- None relevant: `` (empty string between escape tags)

**Selection Criteria:**

Select a candidate if understanding it helps answer the topic query:
- Domain types that carry meaningful data (ScoreConfig, EdgeContext)
- Types that affect behavior (Config, Options, Strategy)
- Core abstractions the symbol operates on

Skip a candidate if it's:
- Infrastructure/boilerplate (IO, Maybe, Either, Text, Int)
- Logging/observability (Logger, Tracer, Span)
- Generic wrappers that don't add domain meaning
- Already well-understood primitives

## Negative Examples (Critical!)

**15-20% of examples should have empty or partial selections.** This prevents FunctionGemma from learning to always select everything.

Cases where `selected` should be empty:
- All candidates are generic types (IO, Maybe, Text)
- The signature is simple with no domain-specific types
- Candidates are infrastructure, not domain logic

Cases where `selected` should be partial:
- Some candidates are relevant, others are boilerplate
- Logger/IO mixed with domain types - skip the Logger/IO

## Example Annotations

### Example 1: Full Selection
```
Symbol: runExplore
Signature: runExplore :: ExploreConfig -> LSPSession -> Query -> IO ExploreResult
Candidates: ExploreConfig, LSPSession, Query, ExploreResult

Topic: "How does code exploration work and what inputs does it need?"
Selected: ExploreConfig,LSPSession,Query,ExploreResult
```
All candidates are domain-relevant.

### Example 2: Partial Selection
```
Symbol: logScore
Signature: logScore :: Logger -> ScoreResult -> IO ()
Candidates: Logger, ScoreResult, IO

Topic: "How are scoring results logged?"
Selected: ScoreResult
```
Logger and IO are infrastructure; only ScoreResult carries domain meaning.

### Example 3: Empty Selection
```
Symbol: readConfig
Signature: readConfig :: FilePath -> IO (Maybe Text)
Candidates: FilePath, IO, Maybe, Text

Topic: "How is configuration loaded from disk?"
Selected:
```
All candidates are standard library types with no domain-specific meaning.

## Process

1. Read each JSONL line
2. Parse the `text` field to understand the symbol
3. Look at the signature and candidates
4. Write a natural topic query
5. Select relevant candidates (or none)
6. Replace both holes in the text
7. Output the modified JSONL line

## Output Format

Output valid JSONL with holes replaced. Each line should be valid JSON with a `text` field containing the filled template.

**Important:** Preserve exact formatting - same escape tags, same whitespace, same structure. Only replace the hole markers.
