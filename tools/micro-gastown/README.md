# Micro-Gas Town: Sleeptime Evolution System

Lightweight Haiku-only agent town for automated DSL evolution based on production logs.

## Overview

Micro-Gas Town is a specialized multi-agent system that observes ExoMonad graph execution via Grafana/Loki logs, identifies patterns (failures, slow calls, repeated errors), and proposes targeted improvements to the DSL.

```
┌────────────────────────────────────────────────────────────────────┐
│                      Micro-Gas Town                                 │
│                                                                     │
│   ┌──────────────┐    ┌──────────────────┐    ┌──────────────┐    │
│   │ Log Analyzer │───►│ Conditional      │───►│ PR Filer     │    │
│   │              │    │ Refiner          │    │              │    │
│   │ Queries Loki │    │ Drafts changes   │    │ Commits PR   │    │
│   │ for patterns │    │ to DSL           │    │ with evidence│    │
│   └──────────────┘    └──────────────────┘    └──────────────┘    │
│          ▲                                             │           │
│          │                                             │           │
│          └─────────── Feedback Loop ───────────────────┘           │
│                                                                     │
└────────────────────────────────────────────────────────────────────┘
                              │
                              ▼
              ┌───────────────────────────────┐
              │     Grafana/Loki Logs         │
              │     (via sleeptime-logs CLI)  │
              └───────────────────────────────┘
```

## Why Haiku-Only?

1. **Cost efficiency**: Sleeptime evolution runs continuously - Haiku is ~20x cheaper than Opus
2. **Latency**: Log analysis doesn't need deep reasoning - fast is better
3. **Simplicity**: Smaller agents mean simpler coordination
4. **Iteration speed**: Faster cycles = more evolution attempts

## Polecats

### Log Analyzer

Queries Grafana/Loki via `sleeptime-logs` CLI to find patterns:

- Failed graph transitions
- Slow LLM calls (latency > threshold)
- Repeated errors
- Underutilized paths

**Input**: Time range, query type
**Output**: Structured pattern report (JSON)

### Conditional Refiner

Receives pattern reports and drafts targeted changes:

- Template adjustments for high-latency prompts
- Schema refinements for parsing failures
- Graph restructuring for dead paths
- Tool improvements for repeated errors

**Input**: Pattern report from Log Analyzer
**Output**: Proposed changes (unified diff + rationale)

### PR Filer

Commits proposed changes with log evidence:

- Creates branch with descriptive name
- Includes log snippets as evidence
- Links to relevant Grafana queries
- Opens PR for human review

**Input**: Proposed changes from Refiner
**Output**: PR URL

## Formula System

Polecats are defined via `.formula.toml` files that specify:

```toml
[polecat]
name = "log-analyzer"
description = "Queries Loki for execution patterns"

[prompt]
system = """
You analyze ExoMonad graph execution logs to find patterns.
Focus on: failures, latency spikes, repeated errors.
"""

[tools]
available = ["sleeptime_logs_query", "pattern_report"]

[model]
provider = "anthropic"
name = "claude-haiku-4-20250514"

[output]
schema = "PatternReport"
```

## Integration with Observability

Micro-Gas Town consumes events published by the Observability effect:

| Event Type | What We Learn |
|------------|---------------|
| `graph_transition` | Which paths fail, which are underused |
| `llm_call` | Which prompts are slow, token usage patterns |
| `effect_execution` | Which effects fail, latency distribution |
| `error` | Recurring error patterns, root causes |

See `tools/sleeptime-logs/` for the CLI that queries these events.

## Usage

```bash
# Run the full sleeptime evolution cycle
micro-gastown evolve --since 24h

# Run just the analyzer
micro-gastown analyze --since 24h --output patterns.json

# Run refiner on existing patterns
micro-gastown refine --input patterns.json --output changes.diff

# File a PR from changes
micro-gastown file-pr --input changes.diff --evidence patterns.json
```

## Directory Structure

```
tools/micro-gastown/
├── README.md           # This file
├── polecats/           # Polecat definitions
│   ├── log-analyzer.toml
│   ├── conditional-refiner.toml
│   └── pr-filer.toml
├── formulas/           # Skill formulas
│   ├── analyze.formula.toml
│   ├── refine.formula.toml
│   └── file-pr.formula.toml
├── schemas/            # JSON Schema definitions
│   ├── PatternReport.json
│   ├── ProposedChanges.json
│   └── PRResult.json
└── docs/skills/        # Skill documentation
    ├── query-patterns.md
    ├── draft-changes.md
    └── file-evidence.md
```

## Future Work

- [ ] Automated scheduling (cron trigger)
- [ ] A/B testing of proposed changes
- [ ] Rollback detection (new errors after merge)
- [ ] Cross-graph pattern analysis
- [ ] Synthetic benchmark generation
