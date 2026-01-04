# Gas Town LSP Integration

> Design document for integrating LSP capabilities with Gas Town agents.

## Overview

Gas Town agents (polecats, witnesses, mayor) can leverage LSP (Language Server
Protocol) for code intelligence. This enables:

- **Polecats**: Understand code before making changes (types, references, definitions)
- **Witnesses**: Validate polecat work via diagnostics (type errors, unused imports)
- **Mayor**: Make architecture decisions with code structure awareness

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Gas Town Agent (Claude Code)                  │
│                                                                      │
│  ┌────────────────┐     ┌─────────────────┐     ┌────────────────┐  │
│  │ LSP Tool       │────▶│ HLS Process     │────▶│ Haskell Code   │  │
│  │ (built-in)     │     │ (subprocess)    │     │ Analysis       │  │
│  └────────────────┘     └─────────────────┘     └────────────────┘  │
│         ▲                                                            │
│         │ ENABLE_LSP_TOOL=1                                         │
│         │                                                            │
│  ┌────────────────┐                                                  │
│  │ Agent Session  │◀──────── gt polecat spawn                        │
│  │ Environment    │                                                  │
│  └────────────────┘                                                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Integration Points

### 1. Environment Variable: `ENABLE_LSP_TOOL=1`

Claude Code's built-in LSP tool requires this environment variable to be set.

**Current status**: Not set by default in Gas Town polecat sessions.

**Solution**: Set in polecat spawn or via shell profile.

### 2. HLS Plugin for Claude Code

For Haskell projects, the recommended plugin is `claude-hls`:

```bash
/plugin marketplace add m4dc4p/claude-hls
/plugin install hls@claude-hls
```

This provides:
- Go-to-definition
- Find references
- Hover information (types, documentation)
- Diagnostics (type errors, warnings)
- Code completions

### 3. Project Configuration

HLS requires project detection. Ensure your project has:
- `cabal.project` file (for Cabal projects)
- `stack.yaml` file (for Stack projects)
- `hie.yaml` file (for custom configuration)

## Configuration Changes

### Option A: User-level (affects all polecats for this user)

Add to `~/.bashrc` or `~/.zshrc`:

```bash
export ENABLE_LSP_TOOL=1
```

### Option B: Project-level hook

Add to `.claude/settings.json` in the project:

```json
{
  "hooks": {
    "SessionStart": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "export ENABLE_LSP_TOOL=1"
          }
        ]
      }
    ]
  }
}
```

**Note**: Environment variable exports in hooks may not persist. Prefer Option A.

### Option C: Gas Town spawn configuration (recommended)

Modify the Gas Town polecat spawn to set the environment variable:

```go
// In gastown/internal/polecat/manager.go (conceptual)
env := os.Environ()
env = append(env, "ENABLE_LSP_TOOL=1")
cmd.Env = env
```

## Agent Use Cases

### Polecat: Understanding Code Before Changes

```
# Before editing a function, understand its type and usage
LSP operation: hover on function name → get type signature
LSP operation: findReferences → see all call sites
LSP operation: goToDefinition → understand implementation
```

**Example workflow**:
1. Polecat is assigned a task: "Add error handling to processInput"
2. Polecat uses LSP hover to understand `processInput`'s type
3. Polecat uses LSP references to find all callers
4. Polecat makes informed changes knowing the impact

### Witness: Validating Polecat Work

```
# After polecat commits, check for type errors
LSP operation: diagnostics on changed files → get errors/warnings
```

**Example workflow**:
1. Polecat signals DONE
2. Witness runs `gt verify` which includes LSP diagnostics
3. If diagnostics show errors → REWORK_REQUEST
4. If clean → MERGE_READY to Refinery

### Mayor: Architecture Decisions

```
# Understand codebase structure for strategic decisions
LSP operation: workspaceSymbol → find all types/functions matching pattern
LSP operation: documentSymbol → get file structure
```

**Example workflow**:
1. Mayor needs to decide where to add a new feature
2. LSP workspaceSymbol finds related modules
3. LSP documentSymbol shows structure of candidate files
4. Mayor makes informed dispatch decision

## LSP Operations Available

| Operation | Description | Use Case |
|-----------|-------------|----------|
| `goToDefinition` | Find where a symbol is defined | Understanding implementation |
| `findReferences` | Find all references to a symbol | Impact analysis |
| `hover` | Get type info and documentation | Understanding types |
| `documentSymbol` | Get all symbols in a document | File structure overview |
| `workspaceSymbol` | Search symbols across workspace | Finding related code |
| `goToImplementation` | Find implementations of interface | Understanding abstractions |
| `incomingCalls` | Find callers of a function | Impact analysis |
| `outgoingCalls` | Find callees of a function | Dependency analysis |

## Implementation Checklist

- [ ] Set `ENABLE_LSP_TOOL=1` in user's shell profile
- [ ] Install HLS plugin: `/plugin marketplace add m4dc4p/claude-hls`
- [ ] Verify HLS is installed: `which haskell-language-server-wrapper`
- [ ] Ensure project has `cabal.project` or `stack.yaml`
- [ ] Test LSP with a simple operation: `LSP hover` on a function

## Gas Town CLI Enhancement (Future)

A `gt lsp` command could wrap LSP operations for non-Claude-Code contexts:

```bash
# Proposed CLI
gt lsp hover file.hs:10:5
gt lsp references file.hs:10:5
gt lsp diagnostics file.hs
```

This would use the Tidepool LSP executor to spawn HLS and query it.

## Troubleshooting

### "No LSP server available for file type: .hs"

1. Check `ENABLE_LSP_TOOL` is set: `echo $ENABLE_LSP_TOOL`
2. Restart Claude Code after setting the variable
3. Install HLS plugin if not already installed

### HLS not starting

1. Check HLS is installed: `which haskell-language-server-wrapper`
2. Check project detection: ensure `cabal.project` exists
3. Try building the project first: `cabal build all`

### Slow LSP responses

HLS needs to index the project on first run. This can take several minutes
for large projects. Subsequent queries are fast.

## References

- [Claude Code LSP Documentation](https://docs.anthropic.com/claude-code/lsp)
- [HLS Plugin for Claude Code](https://github.com/m4dc4p/claude-hls)
- [Tidepool LSP Effect](../tidepool-native-gui/lsp-executor/src/Tidepool/LSP/)
- [Gas Town Architecture](https://github.com/steveyegge/gastown)
