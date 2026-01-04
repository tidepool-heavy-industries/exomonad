# Draft Changes Skill

Skill for drafting targeted DSL improvements based on pattern analysis.

## Overview

The Conditional Refiner polecat uses this skill to analyze patterns from
the Log Analyzer and propose minimal, targeted changes to the graph DSL.

## Usage

```bash
# Draft changes from pattern report
micro-gastown refine --patterns patterns.json

# Preview without generating diff
micro-gastown refine --patterns patterns.json --dry-run

# Limit scope
micro-gastown refine --patterns patterns.json --max-changes 3
```

## Change Types

### Template Refinements

**For patterns**: `slow_llm` (high token count)

**Approach**:
1. Identify template file from node definition
2. Find verbose sections (e.g., full histories, redundant context)
3. Remove or summarize verbose sections
4. Add comment explaining removal

**Example**:
```diff
--- a/tidepool-dm/templates/dm_scene.jinja
+++ b/tidepool-dm/templates/dm_scene.jinja
@@ -15,8 +15,6 @@
 ## Scene Context

 {{ scene_description }}
-
-{{ full_faction_history }}  {# REMOVED: reduces prompt by ~800 tokens #}

 ## Current Situation
```

### Schema Adjustments

**For patterns**: `failed_transition` (parsing failures)

**Approach**:
1. Examine error messages for parsing hints
2. Relax overly strict schema constraints
3. Add optional fields for edge cases
4. Improve field descriptions

**Example**:
```diff
--- a/tidepool-dm/src/DM/Schema.hs
+++ b/tidepool-dm/src/DM/Schema.hs
@@ -25,7 +25,7 @@ data SceneNarrative = SceneNarrative
   { snDescription :: Text
-  , snMood :: Mood  -- LLM sometimes returns invalid mood
+  , snMood :: Maybe Mood  -- Made optional to handle edge cases
   , snNPCs :: [NPCAction]
   }
```

### Graph Restructuring

**For patterns**: `dead_path`, `failed_transition` (structural issues)

**Approach**:
1. Identify unreachable or problematic edges
2. Propose edge additions/removals
3. Suggest node merging for redundant paths
4. Add fallback routes for failure cases

**Example**:
```diff
--- a/tidepool-dm/src/DM/Graph.hs
+++ b/tidepool-dm/src/DM/Graph.hs
@@ -45,7 +45,8 @@ data DMGraph mode = DMGraph
   , dmAction :: mode :- LogicNode
       :@ Needs '[SceneNarrative]
-      :@ UsesEffects '[Goto "dmAftermath" Outcome]
+      :@ UsesEffects '[Goto "dmAftermath" Outcome, Goto "dmScene" SceneNarrative]
+      -- Added fallback to dmScene for retry on dice failure
   , dmAftermath :: mode :- LLMNode
```

## Validation Requirements

All proposed changes must:

1. **Compile**: `cabal build` must succeed
2. **Pass tests**: `cabal test` must pass
3. **Be minimal**: Change only what's necessary
4. **Cite patterns**: Reference the pattern(s) being addressed
5. **Include rationale**: Explain why this change helps
6. **Predict improvement**: Estimate expected metric improvement

## Guardrails

The Refiner has several safety restrictions:

| Guardrail | Value | Reason |
|-----------|-------|--------|
| Max files changed | 5 | Limit blast radius |
| Max lines per file | 50 | Encourage small changes |
| Forbidden paths | Effect/, handlers/ | Don't touch core infra |
| Required: pattern citation | true | No speculative changes |

## Output Format

```diff
--- a/path/to/file.hs
+++ b/path/to/file.hs
@@ -N,M +N,M @@
 context line
-removed line
+added line
 context line


--- RATIONALE ---
Pattern: slow_llm at dmScene (id: slow-llm-dmScene)
Occurrences: 12 in last 24h
Avg latency: 4200ms, Avg tokens: 3800

Change: Removed full_faction_history from template.
Reason: Factions are already summarized in active_threats.
        This duplication adds ~800 tokens with no benefit.

Expected improvement:
- Latency: -15% to -25% (fewer tokens to process)
- Tokens: -800 per call (direct removal)

Verification: Check dmScene latency after deployment.
Rollback: Revert this commit if latency increases.
```

## Example Session

```
$ micro-gastown refine --patterns patterns.json

Loading patterns.json (3 patterns)...

Processing pattern: slow_llm at dmScene
  - Reading template: tidepool-dm/templates/dm_scene.jinja
  - Identified verbose section: full_faction_history (est. 800 tokens)
  - Proposing removal with comment

Processing pattern: slow_llm at dmAftermath
  - Reading template: tidepool-dm/templates/dm_aftermath.jinja
  - No obvious reductions found
  - Skipping (requires manual review)

Processing pattern: failed_transition dmAction->dmScene
  - Reading graph: tidepool-dm/src/DM/Graph.hs
  - Issue: Missing fallback path
  - Proposing additional Goto edge

Validating changes...
  - cabal build: SUCCESS
  - cabal test: SUCCESS

Generated changes.diff (2 files, 12 lines changed)
```
