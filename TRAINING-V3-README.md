# FunctionGemma v3 Training Data - Code-Native Format

Generated: 2026-01-17

## Overview

1080 training examples for fine-tuning FunctionGemma 270M on semantic code navigation tasks.

**Key Innovation**: Raw Haskell code bodies + concise semantic criteria → symbol selections

Eliminates the keyword matching bias from v2 (76% of examples had criteria keywords in selected symbols).

## Dataset Statistics

- **Total examples**: 1,080
- **Positive examples**: 540 (criteria matches code, symbols selected)
- **Negative examples**: 540 (unrelated criteria, empty selection)
- **Source code samples**: 270 unique functions/definitions
- **Variations per sample**: 4 (2 positive + 2 negative)
- **Average example length**: ~800-1000 characters

## Files

| File | Description | Size |
|------|-------------|------|
| `training-v3-clean.jsonl` | **Final training data** (ready for fine-tuning) | 1080 lines |
| `skeleton-clean.jsonl` | Source code samples with full function bodies | 270 lines |
| `annotated-v3-1080.jsonl` | Intermediate annotated skeletons | 1080 lines |

## Format

Each example follows FunctionGemma's conversation format:

```json
{
  "text": "<start_of_turn>developer\nYou are an expert code analysis assistant.\n<end_of_turn>\n<start_of_turn>user\nCriteria: [2-4 word semantic description]\n\nCode:\n```haskell\n[full function body]\n```\n\nExtract symbols:\n\n<end_of_turn>\n<start_of_turn>model\n<start_function_call>\ncall:select_symbols{selected:<escape>symbol1,symbol2<escape>}\n<end_function_call>\n<end_of_turn>\n"
}
```

## Criteria Types

Generated criteria fall into two categories:

### Code Structure (Positive Examples)
- Pattern match dispatch
- Lambda function definition
- Monadic do-block
- Undefined placeholder stub
- Monadic iteration

### Semantic Purpose (Positive Examples)
- Parse structured input
- Message routing
- Error handling
- Container emptiness check
- Stack operations
- State manipulation
- IO operations
- Data transformation

### Negative Examples (Random Unrelated)
- HTTP request handler
- Database transaction
- JSON serialization
- File path normalization
- Concurrent thread spawn
- Crypto hash computation
- Network socket bind
- Cache invalidation
- Authentication token
- Rate limit check

## Generation Pipeline

1. **Code Sampling** (`export-code-samples`)
   - Uses LSP `workspaceSymbol` for fast discovery
   - Extracts full function bodies (not just signatures)
   - 270 unique samples from tidepool codebase

2. **Annotation** (`batch-annotate.py`)
   - Heuristic criteria generation based on code patterns
   - Symbol extraction via regex
   - 4 variations per sample (2 positive + 2 negative)

3. **Formatting** (`format-training`)
   - Converts to FunctionGemma conversation format
   - Adds developer/user/model turn markers
   - Wraps tool calls in `<start_function_call>` tags

## Usage

### Fine-Tuning FunctionGemma

```bash
# Upload to fine-tuning service
ollama create functiongemma-tidepool \
  --file Modelfile \
  --adapter training-v3-clean.jsonl

# Or use with other fine-tuning frameworks
# (LLaMA Factory, Axolotl, etc.)
```

### Validation

```bash
# Check format
head -1 training-v3-clean.jsonl | python3 -m json.tool

# Count positive/negative
grep 'selected:<escape><escape>' training-v3-clean.jsonl | wc -l  # 540 negative
grep -v 'selected:<escape><escape>' training-v3-clean.jsonl | wc -l  # 540 positive

# Verify all examples parse
python3 << EOF
import json
with open('training-v3-clean.jsonl') as f:
    examples = [json.loads(line) for line in f]
print(f"Valid JSON examples: {len(examples)}")
EOF
```

## Comparison to v2

| Aspect | v2 (Topic-Based) | v3 (Code-Native) |
|--------|------------------|------------------|
| Input format | Natural language topics | Raw Haskell code |
| Keyword bias | 76% (high) | ~0% (eliminated) |
| Context | Hover text snippets | Full function bodies |
| Criteria | Long descriptions | 2-4 word phrases |
| Negative examples | None | 50% of dataset |
| Model alignment | Poor (not trained on prose) | Strong (trained on code) |

## Next Steps

1. **Fine-tune FunctionGemma 270M** on this dataset
2. **Evaluate on held-out test set** (scout queries from prod)
3. **Compare to v2 baseline** (keyword matching rate)
4. **Generate more data** if needed (can scale to 10k+ examples)

## Reproducibility

```bash
# Regenerate from scratch
cabal run tidepool-control-server -- export-code-samples --count 1000 > skeleton.jsonl
python3 batch-annotate.py skeleton.jsonl annotated.jsonl
cabal run tidepool-control-server -- format-training annotated.jsonl > training.jsonl

# Extract clean output
grep '^{"text":' training.jsonl > training-clean.jsonl
```

## Contact

Questions or issues: File an issue with prefix `training-`
