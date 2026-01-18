# Task 05: End-to-End MCP Testing with Claude Code

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** [04-integrate-teaching.md](04-integrate-teaching.md)
**Blocks:** 06

## Goal

Test the complete flow with Claude Code as the MCP client:
1. Claude Code calls `teach` MCP tool
2. Control-server runs DocGen graph
3. Graph makes Haiku calls (captured by teaching)
4. JSONL training data is generated
5. Results return to Claude Code

This validates the entire pipeline works end-to-end.

## Context

### Architecture Under Test

```
┌─────────────────────────────────────────────────────────────┐
│ Claude Code (user's TTY)                                    │
│   "What types are involved in the scoring system?"          │
└───────────────────────────┬─────────────────────────────────┘
                            │ MCP: tools/call "teach"
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ mantle-agent mcp (JSON-RPC stdio)                           │
│   Forwards to control-server via TCP                        │
└───────────────────────────┬─────────────────────────────────┘
                            │ TCP NDJSON
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ control-server                                              │
│   → handleTeachTool                                         │
│   → runDocGenGraph                                          │
│   → Graph: dgInit → dgProcess → dgSelect → dgExpand → ...   │
│   → runLLMWithTeaching (records to JSONL)                   │
│   → Returns TeachingDoc                                     │
└───────────────────────────┬─────────────────────────────────┘
                            │
        ┌───────────────────┴───────────────────┐
        ▼                                       ▼
┌───────────────────┐               ┌───────────────────────┐
│ .tidepool/training│               │ MCP Response          │
│ session-*/        │               │ TeachingDoc JSON      │
│   anthropic.jsonl │               │ → Claude Code         │
└───────────────────┘               └───────────────────────┘
```

## Acceptance Criteria

- [ ] **Claude Code can discover `teach` tool** via MCP tools/list
- [ ] **Claude Code can call `teach` tool** with query arguments
- [ ] **Response contains TeachingDoc** with core/prereqs/support
- [ ] **JSONL file created** with TeachingTurn records
- [ ] **Logs visible** in control-server pane showing graph execution
- [ ] **Multiple calls accumulate** in same session JSONL
- [ ] **Claude Code can interpret results** and answer user's question

## Subtasks

### 5.1 Setup Test Environment

**Terminal layout (Zellij 2-pane):**
```
┌─────────────────────────────┬─────────────────────────────┐
│ Pane 1: Claude Code         │ Pane 2: control-server      │
│                             │                             │
│ $ claude                    │ $ TEACHING_ENABLED=true \   │
│                             │   ANTHROPIC_API_KEY=... \   │
│                             │   cabal run                 │
│                             │   tidepool-control-server   │
└─────────────────────────────┴─────────────────────────────┘
```

**Claude Code config** (`.claude/settings.local.json`):
```json
{
  "mcpServers": {
    "tidepool": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_CONTROL_HOST": "127.0.0.1",
        "MANTLE_CONTROL_PORT": "7432"
      }
    }
  }
}
```

### 5.2 Test Tool Discovery

In Claude Code, verify the tool is available:
```
User: What MCP tools do you have access to?

Claude: I have access to the following tools:
- tidepool:teach - Explore code semantics and generate teaching documents
  ...
```

### 5.3 Test Basic Call

```
User: Use the teach tool to explore the scoring system starting from compositeScore

Claude: I'll use the teach tool to explore...
[Calls teach with topic="scoring system", seeds=["compositeScore"], budget=10]

Response: TeachingDoc with:
- core: [compositeScore, ...]
- prereqs: [ScoreConfig, EdgeContext, ...]
- support: [...]
```

### 5.4 Verify JSONL Output

While Claude Code is running, check the output:
```bash
# In another terminal
watch -n 1 'cat .tidepool/training/session-*/anthropic.jsonl | jq -c ".ttNodeName"'

# Should see "dgSelect" entries appearing as exploration runs
```

### 5.5 Test Multiple Calls

```
User: Now explore the LSP integration starting from lspHover

Claude: [Calls teach again]
...
```

Verify:
- Same session JSONL file grows (not new session)
- Second exploration's turns are appended

### 5.6 Validate Training Data Quality

```bash
# Check all turns have node metadata
cat .tidepool/training/session-*/anthropic.jsonl | jq -c '{node: .ttNodeName, graph: .ttGraphName}'

# Check thinking content is captured
cat .tidepool/training/session-*/anthropic.jsonl | jq '.ttResponse.content[] | select(.type=="thinking") | .thinking[:100]'

# Check tool use (if any)
cat .tidepool/training/session-*/anthropic.jsonl | jq '.ttResponse.content[] | select(.type=="tool_use")'
```

### 5.7 Test Error Handling

Test edge cases:
- **Unknown symbol:** `seeds=["nonexistent"]` → graceful error
- **Empty candidates:** Symbol with no type dependencies → skips LLM call
- **Budget exhaustion:** `budget=1` → stops after one iteration

### 5.8 Document Test Scenarios

Create a test script for reproducibility:

```bash
#!/bin/bash
# test-e2e.sh

# Start server in background
TEACHING_ENABLED=true \
ANTHROPIC_API_KEY=$ANTHROPIC_API_KEY \
TEACHING_OUTPUT_DIR=./test-output \
cabal run tidepool-control-server &
SERVER_PID=$!
sleep 5  # Wait for LSP init

# Test via socat (simulates MCP call)
echo '{"type":"McpToolCall","mcpId":"test1","toolName":"teach","arguments":{"topic":"scoring","seeds":["compositeScore"],"budget":5}}' | \
  socat - TCP:127.0.0.1:7432

# Check output
if [ -f ./test-output/session-*/anthropic.jsonl ]; then
  echo "PASS: JSONL created"
  cat ./test-output/session-*/anthropic.jsonl | jq -c '.ttNodeName'
else
  echo "FAIL: No JSONL output"
fi

# Cleanup
kill $SERVER_PID
```

## Files to Create

| File | Purpose |
|------|---------|
| `test/e2e/test-e2e.sh` | Automated E2E test script |
| `test/e2e/README.md` | Manual test instructions |

## Expected Logs (control-server pane)

```
[Server] Starting LSP session...
[LSP] HLS initialized
[Server] Teaching mode enabled, output: .tidepool/training/session-abc123
[Server] Listening on .tidepool/control.sock

[MCP] Received: teach tool call
[Graph] Entering dgInit with TeachQuery{topic="scoring", seeds=["compositeScore"]}
[Graph] dgInit -> dgProcess
[Graph] Entering dgProcess with ProcessInput{symbol="compositeScore", depth=0}
[LSP] hover: compositeScore -> "compositeScore :: ScoreConfig -> ScoreEdgeOutput -> Double"
[Graph] dgProcess -> dgSelect (candidates: ["ScoreConfig", "ScoreEdgeOutput"])
[Graph] Entering dgSelect (LLM node)
[Teaching] Calling Haiku...
[Teaching] Recorded turn for dgSelect
[Graph] dgSelect -> dgExpand
[Graph] Entering dgExpand with ExpandInput{selected=["ScoreConfig", "ScoreEdgeOutput"]}
[Graph] dgExpand -> dgProcess (frontier: [ScoreConfig@1, ScoreEdgeOutput@1])
...
[Graph] dgFinalize -> dgExit
[MCP] Returning TeachingDoc (3 core, 5 prereqs, 2 support)
```

## Verification Checklist

```
[ ] Server starts with teaching enabled
[ ] Claude Code discovers teach tool
[ ] Basic teach call returns TeachingDoc
[ ] JSONL file created with correct structure
[ ] Multiple calls append to same session
[ ] Thinking content captured (non-empty)
[ ] Error cases handled gracefully
[ ] Logs show graph execution flow
[ ] Test script passes
```

## Notes

- First E2E test may be slow (LSP indexing, Haiku cold start)
- Keep budget low (5-10) for faster iteration during testing
- Check Haiku rate limits if making many calls
