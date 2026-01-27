# V3 Training Data Annotation Guide

## Overview

We have **1,080 skeleton examples** ready for manual annotation in `skeleton-v3-1080-rewrite.jsonl`.

Each example has **unique rewrite tokens** that make LLM-assisted batch editing easy.

## File Structure

**Source**: `skeleton-v3-1080-rewrite.jsonl` (1,080 lines)

Each example:
```json
{
  "example_id": "0001",
  "variation": "positive_structure",  // or positive_semantic, negative_1, negative_2
  "code": "actual Haskell code here...",
  "criteria": "CRITERIA_0001",  // ← REPLACE THIS
  "selected": "SELECTED_0001",  // ← REPLACE THIS with JSON array
  "file": "/path/to/source.hs",
  "name": "functionName",
  "range": {...}
}
```

## Variation Types (4 per code sample)

| Variation | Type | Criteria Focus | Selection |
|-----------|------|----------------|-----------|
| `positive_structure` | Positive | Code structure (pattern match, lambda, do-block) | Select relevant symbols |
| `positive_semantic` | Positive | What it does (parse input, route messages, error handling) | Select relevant symbols |
| `negative_1` | Negative | Unrelated criteria | Empty array `[]` |
| `negative_2` | Negative | Unrelated criteria | Empty array `[]` |

## Rewrite Tokens

Every example has **2 unique rewrite targets**:

1. **`CRITERIA_XXXX`** - Replace with 2-4 word semantic description
2. **`SELECTED_XXXX`** - Replace with JSON array of symbol names

### Examples

**Before:**
```json
{
  "example_id": "0001",
  "variation": "positive_structure",
  "code": "forkHandler interpret handler = NodeHandler $ \\router jsonPayload ->\n  case parseEither parseJSON ...",
  "criteria": "CRITERIA_0001",
  "selected": "SELECTED_0001",
  ...
}
```

**After:**
```json
{
  "example_id": "0001",
  "variation": "positive_structure",
  "code": "forkHandler interpret handler = NodeHandler $ \\router jsonPayload ->\n  case parseEither parseJSON ...",
  "criteria": "pattern match dispatch",
  "selected": ["forkHandler", "router", "case", "parseEither"],
  ...
}
```

## LLM-Assisted Batch Annotation

### Strategy

Use an LLM (Claude, GPT-4) to process batches with targeted find-replace:

```
Find: "CRITERIA_0001"
Replace with: "pattern match dispatch"

Find: "SELECTED_0001"
Replace with: ["forkHandler", "router", "case", "parseEither"]
```

### Prompt Template

```
I have a Haskell code sample that needs annotation for training data.

CODE:
```haskell
[paste code field here]
```

TASK:
1. For variation "positive_structure": Generate a 2-4 word criteria describing the CODE STRUCTURE (e.g., "pattern match dispatch", "lambda function", "monadic do-block")
2. For variation "positive_semantic": Generate a 2-4 word criteria describing WHAT IT DOES semantically (e.g., "parse JSON input", "route messages", "handle errors")
3. For both positive variations: Select the key symbols (identifiers) that implement the criteria
4. For negative variations: Generate unrelated criteria that DON'T match the code (e.g., "HTTP authentication" for a stack function)

OUTPUT FORMAT:
CRITERIA_0001: "pattern match dispatch"
SELECTED_0001: ["forkHandler", "router", "case", "parseEither"]

CRITERIA_0002: "message routing and dispatch"
SELECTED_0002: ["forkHandler", "router", "extractSpawnTargets", "forM_"]

CRITERIA_0003: "database transaction commit"
SELECTED_0003: []

CRITERIA_0004: "render HTML template"
SELECTED_0004: []
```

### Batch Processing

Process in groups of **10 code samples** (40 examples):
1. Extract code from examples 1-40 (first 10 functions × 4 variations)
2. Send to LLM with prompt above
3. LLM returns 40 rewrite pairs
4. Use find-replace to apply all 40 rewrites
5. Verify JSON validity
6. Repeat for next batch

## Quality Guidelines

### Good Criteria (Positive Examples)

**Structure-focused:**
- "pattern match dispatch"
- "monadic do-block"
- "lambda function definition"
- "undefined placeholder stub"
- "list comprehension"

**Semantic-focused:**
- "parse JSON input"
- "route messages to workers"
- "handle connection errors"
- "check container emptiness"
- "format for display"

### Bad Criteria (Avoid)

❌ Too generic: "function definition", "data transformation"
❌ Too long: "parse JSON and route to appropriate handler"
❌ Too specific: "parse JSON using parseEither from aeson library"
❌ Just the function name: "forkHandler"

### Symbol Selection Rules

✅ **Include:**
- Function names (the main function being analyzed)
- Key types and constructors
- Important helper functions called
- Distinctive operators or combinators

❌ **Exclude:**
- Haskell keywords (`case`, `of`, `let`, `where`, `do`, `if`, `then`, `else`)
- Common operators (`$`, `<>`, `++`)
- Generic names (`x`, `y`, `input`, `output`)
- Anything not directly relevant to the criteria

### Negative Examples

Generate **diverse, plausible but wrong** criteria:
- "HTTP request handler" (for a stack function)
- "database transaction" (for string formatting)
- "file path normalization" (for JSON parsing)
- "spawn concurrent threads" (for pure math)

## Verification

After annotation, verify:

```bash
# Check JSON validity
python3 -c "import json; [json.loads(line) for line in open('annotated-v3-1080.jsonl')]"

# Count unique criteria
python3 << 'EOF'
import json
with open('annotated-v3-1080.jsonl') as f:
    criteria = [json.loads(line)['criteria'] for line in f]
print(f"Unique criteria: {len(set(criteria))}")
EOF

# Verify no rewrite tokens remain
grep -c 'CRITERIA_' annotated-v3-1080.jsonl  # Should be 0
grep -c 'SELECTED_' annotated-v3-1080.jsonl  # Should be 0
```

## After Annotation

Once all 1,080 examples are annotated:

```bash
# Format as FunctionGemma training data
cabal run tidepool-control-server -- format-training annotated-v3-1080.jsonl > training.jsonl

# Extract clean output
grep '^{"text":' training.jsonl > training-v3-1080.jsonl

# Verify count
wc -l training-v3-1080.jsonl  # Should be 1080
```

## Progress Tracking

Track progress in batches:

| Batch | Examples | Status |
|-------|----------|--------|
| 1-40 | Functions 1-10 | ⬜ Not started |
| 41-80 | Functions 11-20 | ⬜ Not started |
| ... | ... | ... |

Update this file as you complete batches.

## Estimated Time

- **Per batch** (10 functions, 40 examples): ~15-20 minutes
- **Total** (270 functions, 1080 examples): ~7-9 hours
- **With automation**: ~3-4 hours (LLM does heavy lifting, human verifies)

## Tips

1. **Start with variety**: Pick samples from different files to get a feel for the diversity
2. **Build criteria library**: Reuse good criteria across similar code patterns
3. **Verify as you go**: Check JSON validity after each batch
4. **Take breaks**: Better quality with fresh eyes
5. **Use LLM efficiently**: Batch similar code patterns together
