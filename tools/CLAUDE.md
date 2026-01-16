# tools/ - Root-Level Development Tools

Non-Haskell standalone tools for development and analysis.

**Note**: For Haskell tools (ghci-oracle, sleeptime, training-generator), see `haskell/tools/CLAUDE.md`.

## Tools

### blast-radius.sh

Shell script for quick blast radius analysis of code changes.

```bash
./tools/blast-radius.sh <file-pattern>
```

**Purpose:** Identifies potential impact zones when modifying code, helping understand what might break.

### micro-gastown

Experimental micro-service tooling.

```bash
cd tools/micro-gastown
# See micro-gastown README for usage
```

**Status:** Experimental - not currently integrated into main workflows.

## Related Documentation

- **[haskell/tools/CLAUDE.md](../haskell/tools/CLAUDE.md)** - Haskell standalone tools (ghci-oracle, sleeptime, training-generator)
- **[Root CLAUDE.md](../CLAUDE.md)** - Project overview
