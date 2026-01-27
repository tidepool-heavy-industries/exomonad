# FunctionGemma V3 Training Data Generation

## Overview

FunctionGemma is a 270M parameter code-native model trained to identify **salient symbols** in code - the interesting parts worth exploring next when understanding an unfamiliar codebase.

**V3 Approach (Salience-Only):** The model learns "what's core vs what's boilerplate" from code structure alone, without semantic prompts. This eliminates keyword matching bias from earlier versions.

## The Problem V3 Solves

**V1/V2 had 76% keyword bias:** Training data included semantic criteria like "error handling patterns" alongside code. The model learned string matching: if criteria mentioned "error", select symbols with "error" in the name. Useless for real exploration.

**V3 eliminates criteria entirely:** Just `code → salient symbols`. The model learns structural patterns:
- `parseEither`, `ActorRef`, `LLMEnv` → **interesting** (domain-specific)
- `Just`, `Nothing`, `show`, `map` → **boilerplate** (stdlib noise)
- `case`, `=`, `[`, `]` → **not symbols** (language keywords)

## Training Task

```
Input:  Raw code body (function + type sig + comments)
Output: List of salient symbol names worth LSP exploration

Example:
  Input:  "runLLM env = interpret (\\case CompleteLLM p -> callAPI env p)"
  Output: ["runLLM", "CompleteLLM", "callAPI", "LLMEnv"]
          (NOT: "interpret", "case", "pure" - those are plumbing)
```

## Schema (V3 Salience-Only)

### Skeleton Format (for annotation)

```json
{
  "example_id": "0001",
  "code": "forkHandler interpret handler = ...\n  where ...",
  "file": "/path/to/Fork.hs",
  "name": "forkHandler",
  "range": {
    "start": {"line": 123, "character": 0},
    "end": {"line": 135, "character": 11}
  },
  "selected": ["REPLACE_0001"]
}
```

**Fields:**
- `example_id`: Unique ID (zero-padded 4 digits)
- `code`: Full function body including type sig, where clauses, comments
- `file`: Source file path (for provenance)
- `name`: Function/symbol name
- `range`: LSP Range (for traceability)
- `selected`: Array with replace token `["REPLACE_XXXX"]` or annotated symbols `["parseEither", "ActorRef"]`

### Final Training Format (after annotation)

Same schema, but `selected` contains actual symbol names:

```json
{
  "example_id": "0001",
  "code": "forkHandler interpret handler = ...",
  "file": "/path/to/Fork.hs",
  "name": "forkHandler",
  "range": {...},
  "selected": ["parseEither", "parseJSON", "extractSpawnTargets", "router"]
}
```

**NOT included in V3:**
- ❌ `criteria` field (dropped entirely)
- ❌ `variation` field (no positive/negative distinction needed)
- ❌ Multiple copies of same code with different criteria

## Pipeline

### 1. Generate Skeleton

Uses control-server with LSP (HLS) to extract code bodies from the codebase:

```bash
cd /home/inanna/tidepool-heavy-industries/tidepool

# Generate 270 random code samples
cabal run tidepool-control-server -- export-code-samples --count 270 > skeleton-v3.jsonl
```

**What it does:**
- Starts HLS session (indexes workspace)
- Uses `workspaceSymbol` to discover functions
- Randomly samples N functions
- Reads full code bodies via `readCodeAtRange` (includes where clauses, guards, etc.)
- Outputs JSONL with replace tokens: `"selected": ["REPLACE_0001"]`

**Output:** `skeleton-v3.jsonl` with 270 unannotated examples

### 2. Annotate Skeleton

Replace `REPLACE_XXXX` tokens with actual salient symbols.

**Options:**
- **Manual:** Edit JSONL directly, replacing `["REPLACE_0001"]` with `["symbol1", "symbol2"]`
- **LLM-assisted:** Use Claude to read code and suggest salient symbols
- **Batch Edit:** Use Edit tool with `replace_all=false` for precision bulk updates

**Annotation Guidelines:**
- ✅ Include: Domain types (`ActorRef`, `LLMKind`), user functions (`runLLM`, `parseEither`)
- ❌ Exclude: Stdlib (`Just`, `Nothing`, `show`, `map`), keywords (`case`, `=`, `[`, `]`)
- ❌ Exclude: Qualified stdlib (`T.pack`, `Map.empty`, `forM_`)
- **Goal:** What would you explore next via LSP to understand this code?

**Example annotation:**
```json
{
  "example_id": "0026",
  "code": "allEffectMeta = [\n  EffectMeta \"Log\" Internal FireAndForget,\n  ...\n]\n",
  "name": "allEffectMeta",
  "selected": ["EffectMeta", "Internal", "Yielded", "FireAndForget", "Blocking"]
}
```

### 3. Convert to FunctionGemma Format (Future)

Once annotated, convert to 3-turn Gemma conversation format for fine-tuning:

```bash
# NOT YET IMPLEMENTED for V3 salience-only
# Will need updates to Format.hs formatCodeExample
cabal run tidepool-control-server -- format-training skeleton-v3-annotated.jsonl > training-v3.jsonl
```

**Expected output (future):**
```json
{
  "text": "<start_of_turn>developer\nYou are a code analysis assistant.\n<end_of_turn>\n<start_of_turn>user\nCode:\n```haskell\nallEffectMeta = [...]\n```\n\nExtract salient symbols:\n<end_of_turn>\n<start_of_turn>model\n<start_function_call>\ncall:select_symbols{selected:<escape>EffectMeta,Internal,Yielded<escape>}\n<end_function_call>\n<end_of_turn>"
}
```

## Implementation Details

### Code Extraction (`readCodeAtRange`)

Reads full function bodies with smart boundary detection:

**Boundaries (stops reading when encountered):**
1. **Hard boundaries** (always stop):
   - Section divider: `═══` or `---` (10+ dashes)
   - End of file

2. **Soft boundaries** (only after seeing actual code, not during preamble):
   - Next top-level type signature (`foo :: ...` at column 0)
   - Haddock comment for next definition (`-- |`)

3. **Double blank lines** (any context)

4. **Hard limit:** 30 lines past LSP range

**Why this works:**
- Captures complete function including where clauses and guards
- Stops at next definition without needing perfect parsing
- Preserves comments and type signatures (useful semantic signal)

### Symbol Sampling (`sampleCodeBodies`)

Uses LSP `workspaceSymbol` for efficient discovery:

```haskell
discoverSymbols :: LSPSession -> IO [Text]
-- Queries multiple prefixes: "", "run", "handle", "mk", "to", "parse"...
-- Filters to functions only (SKFunction, SKMethod, SKVariable)
-- Excludes derived instances ($fShow*, etc.)
```

Then randomly samples from discovered symbols.

**Alternative:** Full file traversal with `findHaskellFiles`, but slower.

## Key Modules

| Module | Purpose |
|--------|---------|
| `haskell/control-server/src/Tidepool/Control/Export.hs` | Code extraction, LSP sampling, skeleton generation |
| `haskell/tools/training-generator/src/Tidepool/Training/Types.hs` | Type definitions (SalienceExample) |
| `haskell/tools/training-generator/src/Tidepool/Training/Format.hs` | JSONL formatting (needs V3 update) |
| `haskell/control-server/app/Main.hs` | CLI entry points |

## Related Documentation

- **[haskell/control-server/CLAUDE.md](haskell/control-server/CLAUDE.md)** - Control server overview
- **[haskell/tools/training-generator/CLAUDE.md](haskell/tools/training-generator/CLAUDE.md)** - Training types documentation
- **[CLAUDE.md](CLAUDE.md)** - Project overview

## Historical Context

**V1 (Flat candidates):** LSP hover → extract type names → flat candidate list
- Problem: No structure, mixed inputs/outputs/fields

**V2 (Grouped candidates):** Fields, Inputs, Output, References as groups
- Problem: Still had semantic criteria → keyword matching bias

**V3 (Salience-only):** Raw code → salient symbols, no criteria
- Goal: Pure boilerplate filter, composable with downstream semantic scoring

## Next Steps

1. ✅ Update `Export.hs` to output new schema (drop criteria, add example_id)
2. ✅ Add `SalienceExample` type to training-generator
3. ⏳ Annotate 270 code samples (manual or LLM-assisted)
4. 🔄 Update `Format.hs` to format V3 salience-only examples
5. 🔄 Fine-tune FunctionGemma 270M on annotated data
6. 🔄 Deploy fine-tuned model to semantic-scout via Ollama
