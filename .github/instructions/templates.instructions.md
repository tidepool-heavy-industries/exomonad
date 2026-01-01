---
applyTo: "**/templates/**/*.jinja"
---

# Jinja Template Review Instructions

These are typed Jinja templates validated at compile-time against Haskell context types.

## Critical Issues

**Variable Names**
- Template variables must match fields in the corresponding Haskell context type
- `dm_turn.jinja` uses `DMContext`, `compression.jinja` uses `CompressionContext`, etc.
- Typos in variable names cause compile-time errors

**Include/Extends Paths**
- Included templates must exist
- Paths are relative to templates directory

**Schema Blocks**
- `{% schema %}` blocks define JSON schema for LLM structured output
- Must be valid JSON Schema
- Field names must match Haskell output types

## Jinja Patterns Used

```jinja
{# Context access #}
{{ player.stress }}
{{ clocks | length }}

{# Conditionals #}
{% if precarity == "HangingByThread" %}

{# Loops #}
{% for clock in clocks %}

{# Includes #}
{% include "partials/npc_list.jinja" %}
```
