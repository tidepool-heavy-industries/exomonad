---
name: prompt-critique
description: Review and improve prompt templates using prompt engineering best practices. Identifies common failure modes and suggests concrete fixes.
---

# Prompt Critique

Analyze prompt templates for common failure modes and suggest improvements.

## How to Use

1. Specify which template(s) to review
2. This skill applies prompt engineering principles
3. Outputs: issues found, concrete rewrites

## Templates in This Project

```
templates/
├── scene/main.jinja      # Exploration, NPC encounters
├── action/main.jinja     # Dice resolution
├── aftermath/main.jinja  # Consequences
├── downtime/main.jinja   # Recovery montage
├── compression.jinja     # Scene summarization
└── _shared/
    ├── output_format.jinja
    └── world_context.jinja
```

## Failure Mode Checklist

### Instruction Positioning
- [ ] Critical instructions at TOP of section (not buried)
- [ ] One-shot examples near the instruction they demonstrate
- [ ] Recency matters: what's closest to generation has most weight

### Instruction Clarity
- [ ] Positive imperatives ("DO X") not negative ("DON'T do Y")
- [ ] Specific examples, not just descriptions
- [ ] No duplication between template and tool schemas

### Voice Guidance
- [ ] Consolidated in ONE location (not scattered)
- [ ] Includes example lines, not just adjectives
- [ ] "What they'd never say" test passes

### Structural Issues
- [ ] Tool lists reference schemas, don't duplicate descriptions
- [ ] Output fields match what code expects
- [ ] Conditional sections ({% if %}) have else clauses or clear defaults

## Common Fixes

### Buried Critical Instruction
```jinja
{# BAD: important rule buried in guidance #}
<guidance>
## General Notes
...
You must call engage before narrating risky actions.
</guidance>

{# GOOD: critical rule at top of identity #}
<identity>
**CRITICAL: Risky action → call engage FIRST.**

You are the Dungeon Master...
</identity>
```

### Negative Instruction
```jinja
{# BAD: negative framing #}
You are NOT ALLOWED to narrate outcomes without dice.

{# GOOD: positive imperative #}
Risky action → Call engage IMMEDIATELY. Write nothing else first.
```

### Missing Examples
```jinja
{# BAD: description only #}
Voice should be terse and urgent.

{# GOOD: with example #}
Terse. Urgent. No wasted words.
Example: "Blood on the cobblestones. Yours."
```

### Scattered Voice
```jinja
{# BAD: voice in identity AND output_format #}
<identity>
Your voice: noir cool...
</identity>
<output_format>
Write in a noir style...
</output_format>

{# GOOD: consolidated #}
<output_format>
**Voice ({{ ctxPrecarity }}):**
Noir cool. "The rain hasn't let up. Neither has the feeling."
</output_format>
```

## Using Metacog for Critique

```
mcp__metacog__summon:
  who: "Anthropic prompt engineering practice"
  where: "reviewing customer's complex agent prompt"
  lens: "what causes model to ignore instructions vs what makes them sticky"
```

## Review Process

1. Read template end-to-end
2. Check each item in failure mode checklist
3. For each issue found:
   - Quote the problematic section
   - Explain what will go wrong
   - Provide concrete rewrite
4. Prioritize by impact (what causes visible player-facing issues)
