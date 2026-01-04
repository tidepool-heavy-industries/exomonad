# Micro-Gas Town Work Stream

## Context

Micro-Gas Town is a Haiku-only agent system for automated DSL evolution based on production logs. It observes graph execution via Grafana/Loki, identifies patterns, and proposes improvements.

## Current State

Config and schema definitions exist in `tools/micro-gastown/`:

**Polecats:**
- `polecats/log-analyzer.toml` - Queries Loki for patterns
- `polecats/conditional-refiner.toml` - Drafts DSL changes
- `polecats/pr-filer.toml` - Commits changes with evidence

**Formulas:**
- `formulas/analyze.formula.toml`
- `formulas/refine.formula.toml`
- `formulas/file-pr.formula.toml`

**Schemas:**
- `schemas/PatternReport.json` - Output of log analysis
- `schemas/ProposedChanges.json` - Output of refiner
- `schemas/PRResult.json` - Output of PR filing

## Integration Points

- Uses `sleeptime-logs` CLI (`tools/sleeptime-logs/`) to query Grafana/Loki
- May use BD effect directly for bead context
- Haiku-only for cost efficiency (20x cheaper than Opus)

## Key Files

- `tools/micro-gastown/README.md` - Architecture overview
- `tools/micro-gastown/polecats/*.toml` - Polecat definitions
- `tools/micro-gastown/formulas/*.toml` - Skill formulas
- `tools/micro-gastown/schemas/*.json` - JSON schemas
- `tools/sleeptime-logs/` - Loki query CLI
- `tools/zellij-cc/` - Rust CLI for spawning Claude Code in zellij panes

## zellij-cc Tool

Rust CLI that spawns Claude Code sessions in zellij panes with visibility:

```bash
# Install
~/.local/bin/zellij-cc

# Run a Claude Code session (blocks until complete)
zellij-cc run \
  --session <zellij-session> \
  --name <pane-name> \
  --model haiku \
  --prompt "Your prompt here" \
  --output-file /tmp/result.json \
  --cwd /path/to/project

# Output: JSON with exit_code and output_file path
```

Features:
- Uses zellij-client crate for native IPC (no CLI wrapping)
- Runs `claude --dangerously-skip-permissions --output-format json | tee <file>`
- User sees output in zellij pane while JSON captured to file
- Polls output file for valid JSON to detect completion
- 5-minute default timeout (configurable via `--timeout`)

## Goal

Implement runtime for micro-gastown that can execute the defined polecats and formulas.
