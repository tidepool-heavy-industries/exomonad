# Implementation Tiers

Build in order of increasing complexity. Each tier builds on previous infrastructure.

## Tier 1: Pure LSP + Basic Parsing

**Tools:** 3 tools, ~1-2 days total

1. **show-fields** - Definition + parse record syntax
   - LSP: `definition`, `workspaceSymbol`
   - Parse: Record field declarations `{ field :: Type }`
   - Output: Formatted field list

2. **show-constructors** - Definition + parse sum type syntax
   - LSP: `definition`, `workspaceSymbol`
   - Parse: Constructor declarations `= Con1 | Con2`
   - Output: Formatted constructor list

3. **find-callers** - References + filter heuristics
   - LSP: `findReferences`
   - Parse: Surrounding context (±2 lines)
   - Heuristic: Filter imports, type sigs, comments
   - Output: Filtered call sites

**Value:** Immediate productivity boost, low implementation risk

**Complexity:** Low - mostly string parsing and filtering

---

## Tier 2: LSP + Simple Recursion

**Tools:** 3 tools, ~3-5 days total

4. **find-effects** - Parse signatures + recursive call graph
   - LSP: `definition`, `hover`, `findReferences`
   - Parse: Type constraints from signatures
   - Recursive: Follow function calls, track depth
   - Stop: Recognize interpreters (`runLSP`, etc.)
   - Output: Direct + transitive effects

5. **find-pattern-matches** - References + context classification
   - LSP: `findReferences`
   - Parse: Surrounding context to detect pattern position
   - Classify: Case/of, function args, let/where bindings
   - Output: Pattern sites + exhaustiveness info

6. **find-constructors** - References + context classification
   - LSP: `findReferences`
   - Parse: Expression context vs pattern context
   - Classify: Constructor call vs pattern match vs import
   - Output: Construction sites only

**Value:** High - these solve real pain points (8/10, 6/10)

**Complexity:** Medium - requires context-aware parsing and recursion

---

## Tier 3: LSP + Type Parsing

**Tools:** 3 tools, ~5-7 days total

7. **find-type-instantiations** - References + type inference
   - LSP: `findReferences`, `hover`
   - Parse: Type applications (`@Foo`), constraints, hover text
   - Infer: Type from usage context (field access, etc.)
   - Output: Concrete instantiations grouped by type

8. **find-constrained** - Workspace search + constraint parsing
   - LSP: `workspaceSymbol`, `hover`
   - Parse: Complex constraint syntax, handle `Members '[...]`
   - Filter: Actual functions vs comments/docs
   - Output: Functions with matching constraints

9. **find-field-access** - Grep + type verification
   - LSP: `hover` to verify record type
   - Parse: Field access syntax (dot, lens, record update)
   - Classify: Read vs write
   - Output: Access sites grouped by operation

**Value:** Medium-high - nice to have, lower frequency

**Complexity:** High - requires sophisticated type parsing

---

## Tier 4: LLM-Enhanced (Future)

**After Tier 1-3 are working with heuristics**

Add small LLM (Haiku/Gemini Flash) for semantic classification:

1. **classify-reference-usage** - Improve constructor vs pattern detection
   - Input: Code snippet + symbol name
   - Output: `constructor_call | pattern_match | import | type_sig`
   - Benefit: Better than regex for edge cases

2. **classify-ignored-binding** - Detect data flow bugs
   - Input: Code with `_variable` in pattern
   - Output: `bug | deliberate_ignore` + reasoning
   - Benefit: Find unintentional data drops

3. **classify-effect-usage** - Pattern recognition
   - Input: Function body with effect operations
   - Output: `read_modify_write | initialize | read_only`
   - Benefit: Document common patterns

4. **classify-constraint-need** - Propagation analysis
   - Input: Function + called functions with constraints
   - Output: `needs_constraint | has_interpreter`
   - Benefit: Refactoring guidance

**Cost:** ~$0.001 per classification (Haiku), negligible for local dev

**Benefit:** 95% → 99% accuracy, handles edge cases

---

## Build Order Rationale

### Start with Tier 1 because:
- Quick wins (1-2 days to working tools)
- Validates LSP integration
- Proves value to stakeholders
- Low risk (pure parsing, no complex logic)

### Then Tier 2 because:
- Solves highest pain points (find-effects: 8/10, find-constructors: 6/10)
- Builds on Tier 1 infrastructure
- Introduces recursion and call graphs (needed later)

### Then Tier 3 because:
- Less frequent use (weekly vs daily)
- Requires mature parsing infrastructure
- Can leverage learnings from Tier 1-2

### Finally Tier 4 because:
- Proves heuristics work first
- Identifies specific gaps where LLM adds value
- Avoids premature optimization
- Can measure accuracy improvement (heuristic vs LLM)

---

## Success Metrics

**Tier 1 success:**
- 3 tools working
- 90%+ accuracy on test cases
- <100ms response time for small projects

**Tier 2 success:**
- find-effects handles 5+ call depth
- find-constructors filters 95%+ of noise
- find-pattern-matches detects exhaustiveness

**Tier 3 success:**
- find-type-instantiations works with implicit types
- find-constrained handles complex constraint syntax
- find-field-access distinguishes read/write

**Tier 4 success:**
- LLM improves accuracy by 5%+ over heuristics
- Cost <$0.10 per development session
- Latency <500ms per classification

---

## Timeline Estimate

Assuming 1 developer, 50% time allocation:

- **Week 1-2:** Tier 1 (3 tools)
- **Week 3-4:** Tier 2 (3 tools)
- **Week 5-7:** Tier 3 (3 tools)
- **Week 8+:** Tier 4 (LLM integration)

**Total:** ~2 months to production-ready Tier 1-3, +2 weeks for LLM enhancement

---

## Decision Points

### After Tier 1:
- Do these tools solve real problems? (user feedback)
- Is LSP integration stable? (HLS reliability)
- Should we continue to Tier 2? (ROI assessment)

### After Tier 2:
- Is recursion handling robust? (call depth, cycles)
- Are heuristics accurate enough? (90%+?)
- Do we need Tier 3 or jump to LLM? (cost/benefit)

### After Tier 3:
- Where do heuristics fail? (gather failure cases)
- Is LLM worth the complexity? (accuracy delta)
- Which classifications benefit most from LLM? (prioritize)
