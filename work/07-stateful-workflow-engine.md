# Epic: Stateful Agent Workflow Engine

**Goal:** Transform the ExoMonad control server from a reactive, stateless hook handler into a stateful, persistent workflow engine that can drive agents through complex, multi-turn development lifecycles.

## 1. The Problem: "Workflow Amnesia"

Currently, the `Stop` hook handler (`handleStop`) initializes a fresh `WorkflowState` every time it is called:

```haskell
-- Hook.hs
runStopHookLogic tracer input = do
  -- ...
  let initialWorkflow = WorkflowState
        { wsGlobalStops = 0
        , wsStageRetries = Map.empty  -- <--- Always empty!
        , wsCurrentStage = StageBuild
        -- ...
        }
  -- ... runs graph ...
```

**Consequences:**
1.  **Broken Loop Detection:** Logic like `buildLoopCheck` (which checks `wsStageRetries`) never triggers, because the retry count is reset to 0 on every stop. The agent can loop indefinitely on a build error (masked only by the global `CircuitBreaker`).
2.  **Loss of Context:** The engine doesn't know *why* it's checking the build. Is it the first check? The fifth? Are we verifying a fix?
3.  **Reactive Only:** The engine cannot "plan" ahead or resume interrupted workflows after a server restart.

## 2. The Solution: Persistent Workflow Store

We need to persist `WorkflowState` across hook calls, keyed by `SessionId`.

### Architecture

1.  **`WorkflowStore`**: A global `TVar (Map SessionId WorkflowState)` in `ServerConfig`.
2.  **Lifecycle**:
    *   **Init:** Created in `Main.hs` / `Server.hs`.
    *   **Load:** `handleStop` fetches the existing state for the session (or creates a default).
    *   **Save:** After `runGraph` completes, the final `WorkflowState` is written back to the store.
3.  **Persistence (Optional V2):** Serialize this map to disk (`.exomonad/workflows.json`) to survive server restarts.

### Implementation Plan

1.  **Define Store**:
    ```haskell
    type WorkflowStore = TVar (Map SessionId WorkflowState)
    ```
2.  **Update Config**: Add `workflowStore :: WorkflowStore` to `ServerConfig`.
3.  **Refactor `Hook.hs`**:
    *   In `runStopHookLogic`, read from `WorkflowStore`.
    *   Pass the *restored* state to `runGraph` (via `runState`).
    *   Extract the *final* state from `runGraph` result.
    *   Atomically update `WorkflowStore`.

## 3. Vision: The "Planning" Phase

To support the "User -> TL -> Subagent" flow more robustly, we need structured planning *before* execution.

### `PlanGraph` (New)
A graph for the TL role that runs *before* `spawn_agents`.

*   **Input**: User Request (Epic/Feature).
*   **Nodes**:
    *   `AnalyzeReq`: Understand requirements.
    *   `Decompose`: Break into atomic Tasks.
    *   `RefinePlan`: Interactive loop with User (using `TUITools`).
    *   `Execute`: Calls `spawn_agents` for the defined tasks.
*   **Output**: A persistent `ExecutionPlan` stored in the `WorkflowState`.

### Subagent Reporting
Subagents currently run in isolation. They need to report back to the TL.

*   **New MCP Tool**: `report_status` (or `update_task`).
*   **Flow**:
    1.  Subagent finishes (PR filed).
    2.  Subagent calls `report_status(status="complete", pr_url="...")`.
    3.  Control Server intercepts this call.
    4.  Updates the *Parent Session's* `WorkflowState` (marking task as complete).
    5.  TL is notified (via `Notification` hook or next turn).

## 4. Immediate Steps (Fixing the Bug)

1.  Create `ExoMonad.Control.Workflow.Store` module.
2.  Wire `WorkflowStore` into `ServerConfig`.
3.  Update `Hook.hs` to use the store, fixing the `wsStageRetries` bug.
