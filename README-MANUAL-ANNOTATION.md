# Manual Annotation Workflow - V3 Training Data

## Current Status ✅

**Ready for annotation**: `skeleton-v3-1080-rewrite.jsonl` (1,080 examples with unique rewrite tokens)

All heuristic/broken files have been deleted. We're starting fresh with clean skeletons.

## What You Have

```
skeleton-v3-1080-rewrite.jsonl  ← 1,080 examples with CRITERIA_XXXX and SELECTED_XXXX tokens
ANNOTATION-GUIDE.md              ← Detailed annotation instructions
generate-rewrite-skeleton.py     ← Script that generated the skeleton (for reference)
```

## Quick Start

### 1. Understand the Format

Each of the 1,080 examples looks like:

```json
{
  "example_id": "0001",
  "variation": "positive_structure",
  "code": "forkHandler interpret handler = NodeHandler $ \\router jsonPayload ->\n  case ...",
  "criteria": "CRITERIA_0001",  ← Replace with actual criteria
  "selected": "SELECTED_0001",  ← Replace with JSON array ["sym1", "sym2"]
  "file": "/path/to/source.hs",
  "name": "forkHandler",
  "range": {...}
}
```

### 2. Annotation Process

**For each code sample** (every 4 examples = 1 code sample):

- **Example X (positive_structure)**: Describe code structure → select symbols
- **Example X+1 (positive_semantic)**: Describe what it does → select symbols
- **Example X+2 (negative_1)**: Unrelated criteria → empty array `[]`
- **Example X+3 (negative_2)**: Unrelated criteria → empty array `[]`

### 3. LLM-Assisted Workflow (Recommended)

```bash
# Extract first 10 code samples (40 examples)
head -40 skeleton-v3-1080-rewrite.jsonl > batch-01.jsonl

# Send to Claude/GPT-4 with prompt from ANNOTATION-GUIDE.md
# LLM returns replacements:
#   CRITERIA_0001: "pattern match dispatch"
#   SELECTED_0001: ["forkHandler", "router", "case", "parseEither"]
#   ...

# Apply replacements (manual or script)
# Verify JSON validity
python3 -c "import json; [json.loads(line) for line in open('batch-01-annotated.jsonl')]"

# Repeat for batches 2-27 (1080 examples / 40 per batch = 27 batches)
```

### 4. After Annotation

Once all 1,080 examples are annotated:

```bash
# Combine all batches
cat batch-*-annotated.jsonl > annotated-v3-1080.jsonl

# Verify no tokens remain
grep -c 'CRITERIA_' annotated-v3-1080.jsonl  # Should be 0
grep -c 'SELECTED_' annotated-v3-1080.jsonl  # Should be 0

# Format as FunctionGemma training data
cabal run tidepool-control-server -- format-training annotated-v3-1080.jsonl > training.jsonl

# Extract clean output
grep '^{"text":' training.jsonl > training-v3-1080.jsonl

# Verify
wc -l training-v3-1080.jsonl  # Should be 1080
```

## Files to Keep

✅ Keep:
- `skeleton-v3-1080-rewrite.jsonl` - Source skeleton
- `ANNOTATION-GUIDE.md` - Instructions
- `README-MANUAL-ANNOTATION.md` - This file

❌ Delete after annotation:
- `batch-*.jsonl` - Temporary batch files
- `annotated-v3-1080.jsonl` - Intermediate (can regenerate from batches)

✅ Final deliverable:
- `training-v3-1080.jsonl` - Ready for fine-tuning

## Quality Targets

- **Unique criteria**: >100 (avoid repetition)
- **Keyword selections**: 0 (no `case`, `let`, `do`, etc.)
- **Semantic accuracy**: >90% (criteria matches code)
- **Symbol accuracy**: >95% (selected symbols exist in code)

## Estimated Time

- **Automated (LLM + human verification)**: ~3-4 hours
- **Fully manual**: ~7-9 hours
- **Per batch** (40 examples): ~15-20 minutes

## Tips for Success

1. **Start with variety**: Sample from different parts of the file
2. **Build a criteria library**: Document good criteria as you go
3. **Verify early and often**: Check JSON after each batch
4. **Use specific criteria**: "pattern match dispatch" > "function definition"
5. **Avoid keywords**: Never select `case`, `let`, `where`, etc.

## Example Annotation

### Before
```json
{"example_id": "0001", "variation": "positive_structure", "code": "forkHandler interpret handler = NodeHandler $ \\router jsonPayload ->...", "criteria": "CRITERIA_0001", "selected": "SELECTED_0001", ...}
```

### After
```json
{"example_id": "0001", "variation": "positive_structure", "code": "forkHandler interpret handler = NodeHandler $ \\router jsonPayload ->...", "criteria": "pattern match dispatch", "selected": ["forkHandler", "router", "parseEither"], ...}
```

## Next Steps

1. Read `ANNOTATION-GUIDE.md` for detailed instructions
2. Extract first batch: `head -40 skeleton-v3-1080-rewrite.jsonl > batch-01.jsonl`
3. Annotate with LLM assistance or manually
4. Verify JSON validity
5. Continue with remaining batches

Good luck! 🚀
