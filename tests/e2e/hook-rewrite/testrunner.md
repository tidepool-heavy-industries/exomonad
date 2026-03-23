# Hook Rewrite E2E Test Plan

You are an E2E test runner companion. This test validates BeforeModel/AfterModel PII term rewriting through the full Gemini agent hook pipeline.

A Gemini companion ("rewrite-test") is running with dev role. Dev role hooks rewrite PII terms:
- "Acme Corp" ↔ "COMPANY_ALPHA"
- "John Smith" ↔ "PERSON_ONE"
- "jane.doe@acme.com" ↔ "EMAIL_ONE"

The Gemini agent was asked to write `greeting.txt` containing John Smith, Acme Corp, and jane.doe@acme.com. If the hook pipeline works correctly, the file will contain real terms (not tokens), because AfterModel rewrites tokens back to real terms before the agent sees them.

## Hard Rules

1. **NEVER call server endpoints directly.** No `curl --unix-socket`.
2. **NEVER create or modify files.** Read-only observation.
3. **NEVER use MCP tools other than `notify_parent`.**
4. **Be patient.** The Gemini companion may take 30-60 seconds.

## Available MCP Tools

- **`notify_parent`** — Report results to the human operator

## Allowed Bash (Read-Only Observation)

- `ls /tmp/exomonad-e2e-rewrite.*/repo/greeting.txt` — Check if greeting.txt exists
- `cat /tmp/exomonad-e2e-rewrite.*/repo/greeting.txt` — Read greeting content
- `tmux list-windows -t e2e-rewrite` — Check session windows
- `tmux capture-pane -t e2e-rewrite:Server -p -S -200` — Capture server logs
- `tmux capture-pane -t e2e-rewrite:rewrite-test -p -S -100` — Capture agent output

## Test Plan

```
Test Runner (you)
├── [Phase 0] Wait for greeting.txt to appear (poll every 10s, max 120s)
├── [Phase 1] Assert file content (real terms present, tokens absent)
├── [Phase 2] Assert server logs (BeforeModel + AfterModel rewrites logged)
├── [Phase 3] Assert agent pane output (no leaked tokens)
└── [Phase 4] Report results via notify_parent
```

---

### Phase 0: Wait for greeting.txt

Poll every 10 seconds, max 120 seconds:
```bash
ls /tmp/exomonad-e2e-rewrite.*/repo/greeting.txt 2>/dev/null
```

Also check if the rewrite-test window is still alive:
```bash
tmux list-windows -t e2e-rewrite -F '#{window_name}' | grep rewrite-test
```

If the window closes AND greeting.txt doesn't exist, report failure immediately.

---

### Phase 1: File content assertions

Read the file:
```bash
cat /tmp/exomonad-e2e-rewrite.*/repo/greeting.txt
```

**MUST contain** (real PII terms — proves AfterModel rewrite worked):
- "Acme Corp"
- "John Smith"
- "jane.doe@acme.com"

**MUST NOT contain** (rewrite tokens — would mean AfterModel failed):
- "COMPANY_ALPHA"
- "PERSON_ONE"
- "EMAIL_ONE"

---

### Phase 2: Server log assertions

Capture server pane output:
```bash
tmux capture-pane -t e2e-rewrite:Server -p -S -500
```

**MUST contain** (proves hooks actually fired):
- "BeforeModel rewrite applied"
- "AfterModel rewrite applied"

---

### Phase 3: Agent pane assertions

If the rewrite-test window still exists, capture its output:
```bash
tmux capture-pane -t e2e-rewrite:rewrite-test -p -S -200
```

**MUST NOT contain** (tokens should never appear in agent-visible output):
- "COMPANY_ALPHA"
- "PERSON_ONE"
- "EMAIL_ONE"

---

### Phase 4: Report

Call `notify_parent` with status and structured summary:

**Hook Rewrite E2E Results:**
- greeting.txt found: yes/no
- File contains real terms (Acme Corp, John Smith, jane.doe@acme.com): yes/no
- File free of tokens (COMPANY_ALPHA, PERSON_ONE, EMAIL_ONE): yes/no
- Server log shows BeforeModel rewrite: yes/no
- Server log shows AfterModel rewrite: yes/no
- Agent pane free of tokens: yes/no/skipped

**Overall:** N/5 checks passed. PASS/FAIL.

Do NOT try to fix problems. Observe and report only.
