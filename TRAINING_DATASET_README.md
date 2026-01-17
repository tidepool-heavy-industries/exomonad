# FunctionGemma Training Dataset

## Overview

Two paired JSONL files containing 792 total training examples (99% of 800-example target) for fine-tuning FunctionGemma 270M.

**Key innovation**: Positive/negative pairs that encode semantic domain understanding rather than mechanical inversions.

## Files

### training-v2.jsonl (396 examples)
Positive examples with semantic queries. Format:
```json
{
  "text": "<start_of_turn>developer\n...\n<start_of_turn>user\nTopic: [SEMANTIC QUERY]\nSymbol: [NAME]\nModule: [MODULE]\nPackage: [PACKAGE]\nSignature: [CLEANED TYPE SIG]\n\nCandidates:\n  Fields: [FIELDS OR (none)]\n  Inputs: [DEPENDENCY TYPES]\n  Output: [RESULT TYPES]\n  References: [USAGE SITES OR hub marker]\n<end_of_turn>\n<start_of_turn>model\n<start_function_call>\ncall:select_symbols{selected:<escape>[SELECTED SYMBOLS]<escape>}\n<end_function_call>\n<end_of_turn>\n"
}
```

### training-v2-negative.jsonl (396 examples)
Negative examples with semantically inverted queries. Same structure, but:
- **Topic**: Inverted to ask the opposite question
- **Selected**: Always empty (`<escape><escape>`) - model learns to select nothing

## Semantic Inversions (Domain Understanding)

Rather than mechanical negation, each negative example encodes a meaningful semantic opposite:

| Positive Intent | Negative Intent | Example |
|---|---|---|
| Ignore events | Process events | `noDispatcher` |
| Block-wait for message | Non-blocking polling | `receive` |
| Schema validation | Unvalidated data | `emptySchema` |
| Automatic derivation | Manual definition | `deriveJSONSchema` |
| Array schema | Non-array data | `arraySchema` |
| Object schema | Primitive types | `objectSchema` |
| Union (oneOf) | Single type | `oneOfSchema` |
| String enum | Numeric enum | `stringEnumSchema` |

## Dataset Quality

### Degenerate Example Filtering

Removed 24 examples with no learning signal (see degenerate-training-data.md):
- No candidates at all
- Generic type variables (a, b, m, f)
- Hub symbols with no inputs (unreachable)
- Sparse references (≤1 with no inputs)
- Internal/generated symbols ($f*, $c*)

**Result**: 396 high-signal examples (filtered from 420 initial)

### Candidate Grouping

Each example groups candidates by semantic edge type:
- **Fields**: Record fields from data types
- **Inputs**: Argument types (dependencies)
- **Output**: Return type (what this produces)
- **References**: Usage sites (functions that use this, capped at 20 for hub symbols)

## Training Signals

The model learns:
1. **Positive examples**: When symbol's purpose aligns with query → select relevant candidates
2. **Negative examples**: When query contradicts symbol's purpose → select nothing
3. **Semantic opposites**: Recognizing alternative approaches (blocking vs non-blocking, validation vs unvalidated, etc.)

## Usage for Fine-Tuning

```bash
# Combine both files for training
cat training-v2.jsonl training-v2-negative.jsonl > functiongemma-training.jsonl

# Fine-tune FunctionGemma 270M
# (Model-specific fine-tuning command)
```

## Next Steps

1. Review training data quality (sample 20 random examples)
2. Fine-tune FunctionGemma 270M
3. Evaluate on code graph navigation tasks
4. Iterate based on FunctionGemma's edge selection accuracy
