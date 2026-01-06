# tools/ - Development and Analysis Tools

Standalone tools for development, analysis, and sleeptime workflows.

## Tools

### impact-analysis

Analyzes impact of code changes using LSP to build a dependency graph.

```bash
cabal run impact-analysis -- <args>
```

**Modules:**
- `ImpactAnalysis.Types` - Core types for analysis
- `ImpactAnalysis.Graph` - Dependency graph construction
- `ImpactAnalysis.Analyze` - Analysis logic

**Uses:** LSP via lsp-client, tidepool-core effects

### sleeptime-logs

CLI for querying Grafana/Loki logs during sleeptime evolution workflows.

```bash
cabal run sleeptime-logs -- <args>
```

**Modules:**
- `SleeptimeLogs.CLI` - Command-line interface
- `SleeptimeLogs.Loki` - Loki query API
- `SleeptimeLogs.Config` - Configuration management
- `SleeptimeLogs.Output` - Output formatting

**Purpose:** Query structured logs to analyze graph transitions, LLM calls, and errors during sleeptime evolution.

### blast-radius.sh

Shell script for quick blast radius analysis of code changes.

### micro-gastown

Experimental micro-service tooling.

### zellij-cc

Zellij integration for Claude Code sessions.

## Related Documentation

- [tidepool-core/CLAUDE.md](../tidepool-core/CLAUDE.md) - Graph DSL used by impact-analysis
- [tidepool-native-gui/CLAUDE.md](../tidepool-native-gui/CLAUDE.md) - observability-executor used with sleeptime-logs
- [Root CLAUDE.md](../CLAUDE.md) - Sleeptime concept overview
