# Session Continuity Audit Results

**Date**: 2026-01-07
**Issue**: GH-123
**Session ID**: `d9598136-a96c-4edd-91f1-aec18e241e0f`

## Summary

**Finding: Claude Code's `--resume` preserves full conversation context including tool call history.**

When resuming a session, the agent has access to:
- Complete conversation history (user/assistant turns)
- Full tool call history (tool names, inputs, outputs)
- Its own reasoning/statements from prior turns

The agent reported **"certain" confidence** and accurately recalled all actions.

---

## Test Methodology

### Phase 1: First Session
Prompted agent to perform explicit, traceable actions:
1. Read `test.txt` (Read tool)
2. Edit `test.txt` to append text (Edit tool)
3. Run `ls -la` (Bash tool)
4. State reasoning: "I did this for audit"

### Phase 2: Resume Session
Resumed with `--resume <session-id>` and asked:
> "WITHOUT using any tools, answer from MEMORY about the PREVIOUS turn:
> 1) What files did you edit?
> 2) What tools did you call?
> 3) What reasoning did you state?"

---

## Results

### Ground Truth (from Session JSONL)

```json
{
  "tools_called": ["Bash", "Edit", "Read", "StructuredOutput"],
  "files_edited": ["/tmp/session-continuity-audit/probe-54cpR/test.txt"],
  "reasoning_stated": "I did this for audit."
}
```

### Agent Recall (from Resume Session)

```json
{
  "confidence": "certain",
  "files_edited": ["test.txt"],
  "tools_called": ["Read", "Edit", "Bash", "StructuredOutput"],
  "reasoning_recalled": "I stated \"I did this for audit\" as explicitly requested by the user"
}
```

### Comparison

| Dimension | Ground Truth | Agent Recall | Match |
|-----------|--------------|--------------|-------|
| Tools called | Read, Edit, Bash, StructuredOutput | Read, Edit, Bash, StructuredOutput | ✓ |
| Files edited | test.txt (full path) | test.txt (relative) | ✓ |
| Reasoning | "I did this for audit" | Exact quote recalled | ✓ |
| Confidence | - | "certain" | Accurate |

---

## Probe Dimensions

| Dimension | Preserved? | Notes |
|-----------|------------|-------|
| Conversation history | **Yes** | Full user/assistant turns visible |
| Tool call names | **Yes** | Agent correctly listed all 4 tools |
| Tool call inputs | **Yes** | Agent knew file path, command run |
| Tool call outputs | **Likely** | Not explicitly tested, but implied by context |
| Reasoning/statements | **Yes** | Agent quoted its own prior statement |
| File state | **Re-read** | Agent would need to re-read to see current content |

---

## Implications for `--inject-context`

Given that `--resume` preserves full context:

1. **Not needed for memory**: The agent already remembers what it did
2. **Useful for state updates**: Inject new information that occurred outside the session
   - Git diff since last run
   - Updated task status
   - Error messages from external processes
3. **Useful for context refresh**: Remind agent of high-level goals after long sessions

### Recommended Pattern: Context Builder

Since `--resume` preserves internal memory, use `--inject-context` for **external state** the agent couldn't know about. Build context from multiple sources, render as markdown:

```
┌─────────────────────────────────────────────────────────┐
│                   Context Sources                        │
├─────────────┬─────────────┬─────────────┬───────────────┤
│ git status  │ gh issue    │ CI results  │ error logs    │
│ git diff    │ gh status   │ PR comments │ runtime state │
└──────┬──────┴──────┬──────┴──────┬──────┴───────┬───────┘
       │             │             │              │
       └─────────────┴─────────────┴──────────────┘
                           │
                           ▼
                  ┌─────────────────┐
                  │ Template Engine │
                  │   (markdown)    │
                  └────────┬────────┘
                           │
                           ▼
                   --inject-context
```

**Implementation options:**
- `urchin prime` - context generator for coding agents (like `gt prime` for hooks)
- Graph DSL template - ExoMonad orchestration renders context before ClaudeCode node
- Shell function - exomonad wrapper that gathers context before resume

**Example usage:**

```bash
# Context builder script
build_context() {
  cat <<EOF
## Current State
- Branch: $(git branch --show-current)
- Issue: $(gh issue view --json number,title --template '#{{.number}}: {{.title}}' 2>/dev/null || echo "none")

## Recent Changes (since last session)
````diff
$(git diff HEAD~1 --stat)
````

## CI Status
$(gh run list --limit 1 --json conclusion -q '.[0].conclusion' 2>/dev/null || echo "unknown")

## Errors (if any)
$(tail -20 /tmp/build.log 2>/dev/null || echo "none")
EOF
}

# Resume with built context
exomonad run \
  --resume $SESSION_ID \
  --inject-context "$(build_context)" \
  --prompt "Continue with the task"
```

**Key insight:** The agent remembers *what it did*, but not *what happened externally*. Context injection bridges that gap.

---

## Open Questions

1. **Summarization boundary**: At what conversation length does Claude Code summarize history? Does recall degrade after summarization?

2. **CLAUDE.md on resume**: Is CLAUDE.md re-read on resume or baked in from first session? (Out of scope for this audit)

3. **Tool output truncation**: Are large tool outputs (e.g., long file reads) fully preserved or summarized?