# LSP Tool Pain Points

Real-world workflows from Claude Code sessions exploring the Tidepool codebase.

## Organization

- **high-frequency/** - Daily pain points (5-10+ times per session)
- **medium-frequency/** - Weekly workflows (occasional use)
- **implementation-tiers.md** - Recommended build order
- **top-3.md** - Highest-value tools to build first

## Quick Index

### High-Frequency (Daily)
1. [Where is this type constructed?](high-frequency/01-find-constructors.md) - Pain: 6/10
2. [What effects does this function use?](high-frequency/02-find-effects.md) - Pain: 8/10
3. [Type instantiations](high-frequency/03-find-type-instantiations.md) - Pain: 7/10
4. [What functions call this?](high-frequency/04-find-callers.md) - Pain: 5/10
5. [Show record fields](high-frequency/05-show-fields.md) - Pain: 4/10

### Medium-Frequency (Weekly)
6. [Find pattern matches](medium-frequency/06-find-pattern-matches.md)
7. [Where is this field accessed?](medium-frequency/07-find-field-access.md)
8. [Show sum type constructors](medium-frequency/08-show-constructors.md)
9. [Find functions with constraint](medium-frequency/09-find-constrained.md)

## Key Insight

**Start with heuristics, not LLMs.** Simple pattern matching on context around LSP results gives 90% accuracy for filtering constructor calls vs pattern matches vs imports.

## Implementation Strategy

Build in tiers:
1. **Tier 1**: Pure LSP + basic parsing (show-fields, show-constructors, find-callers)
2. **Tier 2**: LSP + simple recursion (find-effects, find-pattern-matches, find-constructors)
3. **Tier 3**: LSP + type parsing (find-type-instantiations, find-constrained)

Add LLM classification only after getting these working with heuristics.
