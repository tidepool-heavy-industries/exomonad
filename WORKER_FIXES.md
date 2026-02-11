# Worker Reliability Fixes (Gemini-Only Model)

## Goal
Make `spawn_worker` reliable enough to run 10+ Gemini workers in parallel, all completing and reporting back successfully.

## Current Issues

### 1. MCP Connection Failures (CRITICAL)
Workers show:
```
Error: Client is not connected, must connect before interacting with the server.
Current state is disconnected
```

**Debug:**
- Server is running (curl http://localhost:7432/health works)
- Worker config has correct httpUrl: `http://localhost:7432/agents/{name}/mcp`
- But Gemini CLI can't connect during startup

**Possible causes:**
- Race condition: worker starts before server is ready for that specific agent endpoint
- Auth/identity issue: server doesn't recognize the agent ID
- Gemini CLI bug: StreamableHttp transport has initialization issues

**Fix approach:**
1. Add detailed logging to `rust/exomonad/src/main.rs` — log every MCP request with agent ID
2. Check if `/agents/{name}/mcp` route exists and works: `curl -X POST http://localhost:7432/agents/test-gemini/mcp -H 'Content-Type: application/json' -d '...'`
3. Add retry logic in worker spawn? Or is this a Gemini CLI issue?

### 2. Completion Messages Not Arriving (CRITICAL)
Workers exit but `get_agent_messages` returns empty.

**What we know:**
- Worker config has correct `afterAgent` hook
- Hook calls `exomonad hook worker-exit --runtime gemini`
- Haskell handler `handleWorkerExit` calls `sendNote`

**What's broken:**
- Messages never appear in parent inbox
- Either `sendNote` fails silently, or the message goes to wrong location

**Debug:**
1. Check where `sendNote` writes messages:
   - Read `rust/exomonad-core/src/services/inbox.rs` — what's the file path?
   - Is it `.exomonad/agents/{parent_id}/inbox/` or something else?
2. Verify parent agent ID is discoverable:
   - How does `handleWorkerExit` determine the parent?
   - Does it strip `-gemini` suffix? Read from env var?
3. Add debug logging to `sendNote` — log the exact file path it's writing to
4. Manually create a test message file in the inbox, verify `get_agent_messages` reads it

### 3. Hook Path Shows Wrong Binary (LOW PRIORITY)
Error shows `/Users/inannamalick/.cargo/bin/exomonad` but config has just `exomonad`.

**Likely cause:**
- Gemini CLI resolves `exomonad` to full path at first spawn
- Caches that path somewhere
- Later spawns use cached macOS path even though we're on Linux

**Fix:**
- Not critical for functionality (hook still runs, just shows wrong path in error)
- May fix itself after Gemini CLI restart
- If persistent, check Gemini CLI cache dirs

## Priorities

1. **Fix completion messages** — this breaks the entire TL coordination loop
2. **Fix MCP connection** — workers can't use tools without this
3. Ignore hook path cosmetic issue for now

## Next Steps

1. Spawn a Gemini worker with detailed logging enabled
2. Tail the exomonad server logs while it runs
3. After it exits, manually check the inbox directory for the completion message
4. Trace why `get_agent_messages` doesn't see it

## Test Pattern

```bash
# In TL session
spawn_worker name="debug-test" prompt="Echo hello to /tmp/test.txt and exit"

# In another terminal
tail -f ~/.exomonad/logs/server.log  # (or wherever logs go)

# After worker exits
ls -la .exomonad/agents/*/inbox/
cat .exomonad/agents/*/inbox/*  # See if message is there

get_agent_messages  # Does it find the message?
```
