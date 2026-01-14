# tools/ - Development and Analysis Tools

Standalone tools for development, analysis, and sleeptime workflows.

## Tools

### ghci-oracle

Persistent GHCi session server for type queries and expression evaluation. Runs as a standalone process to isolate heavy GHCi dependencies from the main tidepool build.

```bash
cd tools/ghci-oracle
cabal run ghci-oracle -- --port 9999 --project /path/to/project
```

**Modules:**
- `GHCi.Oracle.Server` - Socket server, request dispatch
- `GHCi.Oracle.Session` - GHCi subprocess lifecycle management
- `GHCi.Oracle.Protocol` - Length-prefixed JSON wire protocol
- `GHCi.Oracle.Types` - Wire protocol types

**Purpose:** Enable fast, repeated GHCi queries without spawning new processes. Used by `tidepool-ghci-interpreter` to interpret the `GHCi` effect.

See [ghci-oracle/CLAUDE.md](ghci-oracle/CLAUDE.md) for full documentation.

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

## Related Documentation

- [tidepool-native-gui/CLAUDE.md](../tidepool-native-gui/CLAUDE.md) - observability-interpreter used with sleeptime-logs
- [Root CLAUDE.md](../CLAUDE.md) - Sleeptime concept overview
