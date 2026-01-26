# Cross-Deployment Self-Evolution Protocol

Protocol for lightweight CF graphs to request heavy Docker work, enabling self-modification loops.

## Architecture

**Cloudflare**: Decision-making, lightweight LLM orchestration
**Docker Host**: Heavy code editing, testing, git operations

## Typed Interface

### WorkRequest (CF → Docker)

```typescript
interface WorkRequest {
  requestId: string          // UUID for tracking
  sessionId?: string         // Optional: continue existing session
  slug: string               // Session identifier (e.g., "update-handler/llm")
  model: "sonnet" | "opus" | "haiku"

  task: {
    type: "scaffold" | "implement" | "test" | "refactor" | "fix"
    description: string      // Natural language task description
    acceptanceCriteria: string[]
    targetPath: string       // Where to write code
    testPath?: string        // Where tests live
  }

  context: {
    repoUrl: string          // Git repo to work on
    branch: string           // Target branch
    parentBranch?: string    // For branching strategy
    relevantFiles?: string[] // Files to read first
  }

  constraints?: {
    maxTurns?: number        // Turn limit (default: 20)
    timeout?: number         // Wall-clock timeout in seconds
  }
}
```

### WorkResult (Docker → CF)

```typescript
interface WorkResult {
  requestId: string          // Matches request
  status: "success" | "failure" | "partial"
  sessionId: string          // For continuation

  output: {
    commit?: string          // Git commit hash
    branch: string           // Branch where work was done
    filesModified: string[]  // Changed files
    summary: string          // Human-readable summary
  }

  // Structured output from final decision tool
  structuredOutput: ImplOutput | MergeComplete | ScaffoldOutput

  metrics: {
    turnsUsed: number
    tokensUsed: number
    durationSeconds: number
  }

  // If status !== "success"
  error?: {
    phase: string            // Where it failed
    message: string
    recoverable: boolean
  }
}
```

### Haskell Types (Docker Side)

```haskell
-- In types-first-dev or new package
data WorkRequest = WorkRequest
  { wrRequestId :: Text
  , wrSessionId :: Maybe SessionId
  , wrSlug :: Text
  , wrModel :: ModelChoice
  , wrTask :: TaskSpec
  , wrContext :: RepoContext
  , wrConstraints :: Maybe Constraints
  }

data WorkResult = WorkResult
  { wrRequestId :: Text
  , wrStatus :: WorkStatus
  , wrSessionId :: SessionId
  , wrOutput :: WorkOutput
  , wrStructuredOutput :: Value  -- JSON from decision tool
  , wrMetrics :: WorkMetrics
  , wrError :: Maybe WorkError
  }
```

## Manual Flow (Phase 1)

### 1. CF Graph Requests Work

```typescript
// In CF Durable Object handler
async handleLLMNode(input: SomeInput): Promise<EffectResult> {
  // Lightweight decision making
  const decision = await this.callLLM({
    prompt: "Should we update the LLM handler?",
    schema: DecisionSchema
  });

  if (decision.needsCodeChange) {
    // Construct work request
    const request: WorkRequest = {
      requestId: crypto.randomUUID(),
      slug: "update-llm-handler",
      model: "sonnet",
      task: {
        type: "implement",
        description: decision.changeDescription,
        acceptanceCriteria: decision.criteria,
        targetPath: "src/handlers/llm.ts",
        testPath: "src/handlers/__tests__/llm.test.ts"
      },
      context: {
        repoUrl: "https://github.com/user/exomonad",
        branch: "feat/improve-llm-handler",
        parentBranch: "main"
      }
    };

    // Store request in DO storage
    await this.storage.put(`work-request:${request.requestId}`, request);

    // Log for human to see
    console.log("WORK_REQUEST", JSON.stringify(request, null, 2));

    // Yield to caller (WebSocket client sees this)
    return {
      type: "yield",
      effect: { type: "RequestDockerWork", request }
    };
  }
}
```

### 2. Human Ferries Request

```bash
# Watch CF logs or WebSocket messages
# See WORK_REQUEST JSON

# Save to file
cat > /tmp/work-request.json <<EOF
{...work request JSON...}
EOF

# Execute on Docker host
cd ~/dev/exomonad/types-first-dev
./run-work-request.sh /tmp/work-request.json
```

### 3. Docker Executes

```bash
# run-work-request.sh
#!/usr/bin/env bash
set -euo pipefail

REQUEST_FILE="$1"

# Parse request, extract fields
SLUG=$(jq -r '.slug' "$REQUEST_FILE")
DESCRIPTION=$(jq -r '.task.description' "$REQUEST_FILE")
# ... more parsing

# Run types-first-dev with derived spec
cabal run types-first-dev-exe -- \
  --slug "$SLUG" \
  --description "$DESCRIPTION" \
  --target-path "$(jq -r '.task.targetPath' "$REQUEST_FILE")" \
  --repo-url "$(jq -r '.context.repoUrl' "$REQUEST_FILE")" \
  --branch "$(jq -r '.context.branch' "$REQUEST_FILE")"

# Parse structured output from logs
RESULT=$(tail -1000 .exomonad/logs/*.log | grep STRUCTURED_OUTPUT | tail -1)

# Construct WorkResult
cat > /tmp/work-result.json <<EOF
{
  "requestId": "$(jq -r '.requestId' "$REQUEST_FILE")",
  "status": "success",
  "sessionId": "...",
  "output": {...},
  "structuredOutput": $RESULT,
  "metrics": {...}
}
EOF

echo "Work complete. Result at: /tmp/work-result.json"
```

### 4. Human Ferries Result Back

```bash
# POST result back to CF
curl -X POST https://your-worker.workers.dev/work-result \
  -H "Content-Type: application/json" \
  -d @/tmp/work-result.json
```

### 5. CF Graph Resumes

```typescript
async handleWorkResult(result: WorkResult): Promise<void> {
  // Retrieve original request
  const request = await this.storage.get(`work-request:${result.requestId}`);

  if (result.status === "success") {
    // Work was committed and pushed
    // CI will redeploy us soon

    // Update internal state
    await this.storage.put("last-update", {
      commit: result.output.commit,
      timestamp: Date.now(),
      reason: request.task.description
    });

    // Continue graph execution with result
    return this.dispatch("next-node", result.structuredOutput);
  } else {
    // Handle failure
    console.error("Work failed:", result.error);
  }
}
```

## Automation Path (Phase 2)

Once manual flow works, automate:

### Option A: Webhook from CF

```typescript
// CF emits webhook when it needs work
await fetch("https://your-docker-host/work-queue", {
  method: "POST",
  body: JSON.stringify(request)
});

// Host has HTTP server listening
// Queues work, executes, POSTs result back
```

### Option B: Polling

```bash
# Cron job on Docker host
*/5 * * * * ~/exomonad/check-work-queue.sh

# check-work-queue.sh polls CF endpoint
# Fetches pending work requests
# Executes and returns results
```

### Option C: Message Queue

```typescript
// CF publishes to queue (e.g., Cloudflare Queues)
await env.WORK_QUEUE.send(request);

// Consumer on Docker host
// Processes queue, returns results
```

## Self-Evolution Loop

```
┌────────────────────────────────────────────────────────┐
│                                                        │
│  1. CF graph analyzes own behavior                    │
│     (via logs, traces, errors)                        │
│                                                        │
│  2. Decides improvement needed                        │
│     ("LLM handler too slow")                          │
│                                                        │
│  3. Emits WorkRequest                                 │
│     (task: optimize LLM handler)                      │
│                                                        │
│  4. Docker session modifies src/handlers/llm.ts       │
│     (adds caching, improves prompts)                  │
│                                                        │
│  5. Commits, pushes to branch                         │
│                                                        │
│  6. CI runs tests, deploys to CF                      │
│                                                        │
│  7. New CF graph runs with improvements               │
│                                                        │
│  8. Goto 1 ──────────────────────────────────────────┘
```

## Key Advantages

1. **Separation of Concerns**
   - CF: Fast decisions, state management, always-on
   - Docker: Heavy lifting, filesystem access, git operations

2. **Cost Efficiency**
   - CF charges per request (cheap for LLM-only nodes)
   - Docker only when needed (expensive operations)

3. **Typed Boundaries**
   - WorkRequest/WorkResult enforced at edges
   - Can version protocol independently

4. **Manual → Automated Path**
   - Start with human ferry
   - Validate flow works
   - Add automation incrementally

5. **Self-Modification Safety**
   - Changes go through git (reviewable)
   - CI validates (tests must pass)
   - CF deployment atomic (old version serves until new ready)

## Next Steps

1. **Add WorkRequest/WorkResult to Protocol**
   ```bash
   # Add types to exomonad-generated-ts
   cd haskell/protocol/wire-types
   # Define Haskell types with ToJSON/FromJSON

   # Regenerate TypeScript
   just generate-ts
   ```

2. **Add Yield Effect to CF**
   ```typescript
   // In deploy/src/handlers/index.ts
   case "RequestDockerWork":
     // Store request
     // Log for human
     // Return yield message
   ```

3. **Create run-work-request.sh**
   - Parse WorkRequest JSON
   - Execute types-first-dev
   - Package WorkResult JSON

4. **Test Manual Flow**
   - Deploy simple CF graph
   - Make it emit WorkRequest
   - Ferry manually
   - Verify CF receives result

5. **Iterate Toward Automation**
   - Add webhook endpoint on Docker host
   - CF POSTs requests directly
   - Host POSTs results back

Want to start with step 1 (adding the protocol types) or test the existing deploy setup first?