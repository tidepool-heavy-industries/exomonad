# Plan: Porting DM Agent to V2 Graph DSL

## Executive Summary

Port the DM (Dungeon Master) agent from its current mood-driven state machine implementation to the v2 Graph type-level DSL. The key insight is that **DM moods map naturally to graph nodes**, and **mood transitions map to Goto effects**.

## Current Architecture Analysis

### DM Mood State Machine

```
MoodScene ──engage──► MoodAction ──resolve──► MoodAftermath ──accept──► MoodScene
    │                     │                        │
    │                     │ (empty pool)           │
    │                     ▼                        │
    │              MoodBargain ────────────────────┘
    │                     │
    │                     │ (retreat/pass_out)
    │                     ▼
    └──────────────── BetweenScenes
                          │
                          ▼
                    (new scene) ──► MoodScene
```

### Current Control Flow (`dmTurn`)

1. Record player action as scene beat
2. Build `DMContext` from `WorldState` + current mood
3. Render mood-specific template (via `renderForMood`)
4. Call LLM with template + tools
5. If tool triggers mood transition → **recursive restart** with new mood
6. Apply `TurnOutput` to state
7. Check trauma trigger (stress >= 9 → `MoodTrauma`)
8. Compress if needed
9. Return narrative response

**Key insight**: The "recursive restart" is exactly what `DispatchGoto` provides automatically!

## Proposed Graph Structure

### DMGraph Record Definition

```haskell
data DMGraph mode = DMGraph
  { -- Entry: Player action comes in
    entry :: mode :- G.Entry PlayerInput

    -- Scene variants (exploration phase)
  , sceneEncounter   :: mode :- G.LLMNode
                            :@ Needs '[PlayerInput]
                            :@ Template SceneEncounterTpl
                            :@ Schema TurnOutput
                            :@ UsesEffects DMSceneEffects

  , sceneOpportunity :: mode :- G.LLMNode
                            :@ Needs '[PlayerInput]
                            :@ Template SceneOpportunityTpl
                            :@ Schema TurnOutput
                            :@ UsesEffects DMSceneEffects

  , sceneDiscovery   :: mode :- G.LLMNode
                            :@ Needs '[PlayerInput]
                            :@ Template SceneDiscoveryTpl
                            :@ Schema TurnOutput
                            :@ UsesEffects DMSceneEffects

    -- Action phase (dice resolution)
  , action :: mode :- G.LLMNode
                  :@ Needs '[ActionContext]
                  :@ Template ActionTpl
                  :@ Schema TurnOutput
                  :@ UsesEffects DMActionEffects

    -- Aftermath phase (consequences manifest)
  , aftermath :: mode :- G.LLMNode
                     :@ Needs '[AftermathContext]
                     :@ Template AftermathTpl
                     :@ Schema TurnOutput
                     :@ UsesEffects DMAftermathEffects

    -- Bargain phase (out of dice)
  , bargain :: mode :- G.LLMNode
                   :@ Needs '[BargainContext]
                   :@ Template BargainTpl
                   :@ Schema TurnOutput
                   :@ UsesEffects DMBargainEffects

    -- Trauma phase (stress >= 9)
  , trauma :: mode :- G.LLMNode
                  :@ Needs '[TraumaContext]
                  :@ Template TraumaTpl
                  :@ Schema TurnOutput
                  :@ UsesEffects DMTraumaEffects

    -- Downtime phase (between scenes)
  , downtime :: mode :- G.LLMNode
                    :@ Needs '[DowntimeContext]
                    :@ Template DowntimeTpl
                    :@ Schema TurnOutput
                    :@ UsesEffects DMDowntimeEffects

    -- Router: decides which scene variant to enter
  , sceneRouter :: mode :- G.LogicNode
                       :@ Needs '[SceneSetup]
                       :@ UsesEffects '[Goto "sceneEncounter" PlayerInput
                                       , Goto "sceneOpportunity" PlayerInput
                                       , Goto "sceneDiscovery" PlayerInput]

    -- Exit: Final response to player
  , exit :: mode :- G.Exit Response
  }
  deriving Generic
```

### Effect Types for Each Phase

```haskell
-- Scene nodes can: continue scene, engage (→ action), or complete (→ downtime)
type DMSceneEffects =
  '[ Goto "action" ActionContext      -- engage tool
   , Goto "downtime" DowntimeContext  -- scene ends
   , Goto Self PlayerInput            -- continue in scene
   , Goto Exit Response               -- session ends
   ]

-- Action nodes can: resolve (→ aftermath), or run out of dice (→ bargain)
type DMActionEffects =
  '[ Goto "aftermath" AftermathContext  -- resolve tool
   , Goto "bargain" BargainContext      -- empty dice pool
   , Goto Self ActionContext            -- continue action
   ]

-- Aftermath nodes can: accept (→ scene), or trigger trauma
type DMAftermathEffects =
  '[ Goto "sceneRouter" SceneSetup      -- accept tool
   , Goto "trauma" TraumaContext        -- stress >= 9
   , Goto Self AftermathContext         -- continue aftermath
   ]

-- Bargain nodes can: accept deal (→ scene), retreat (→ downtime), or pass out
type DMBargainEffects =
  '[ Goto "sceneRouter" SceneSetup      -- accept_bargain
   , Goto "downtime" DowntimeContext    -- retreat
   , Goto "trauma" TraumaContext        -- pass_out triggers trauma
   , Goto Self BargainContext           -- continue bargaining
   ]

-- Trauma completes and returns to scene
type DMTraumaEffects =
  '[ Goto "sceneRouter" SceneSetup
   , Goto Exit Response                 -- session ends
   ]

-- Downtime can: start new scene or end session
type DMDowntimeEffects =
  '[ Goto "sceneRouter" SceneSetup
   , Goto Exit Response
   ]
```

## Implementation Tasks

### Phase 1: Define Core Types

1. **Create payload types for transitions**
   - `ActionContext` - what flows from Scene → Action
   - `AftermathContext` - what flows from Action → Aftermath
   - `BargainContext` - what flows when dice pool empties
   - `TraumaContext` - what flows when stress >= 9
   - `DowntimeContext` - what flows for between-scenes
   - `SceneSetup` - what flows to scene router
   - `Response` - final output to player

2. **Refactor DMContext into phase-specific contexts**
   - Current `DMContext` is a union of all phase data
   - Split into `SceneContext`, `ActionContext`, etc.
   - Each template type needs its own context type

3. **Create template definition instances**
   - `SceneEncounterTpl`, `SceneOpportunityTpl`, `SceneDiscoveryTpl`
   - `ActionTpl`, `AftermathTpl`, `BargainTpl`, `TraumaTpl`, `DowntimeTpl`
   - Each implements `TemplateDef` with appropriate context type

### Phase 2: Refactor Tools

Current tools and their graph mapping:

| Tool | Current Behavior | Graph Equivalent |
|------|-----------------|------------------|
| `engage` | Scene → Action transition | `Goto "action"` in scene handler |
| `resolve` | Action → Aftermath transition | `Goto "aftermath"` in action handler |
| `accept` | Aftermath → Scene transition | `Goto "sceneRouter"` in aftermath handler |
| `spend_die` | Player selects die, may trigger Bargain | `RequestInput` effect + conditional `Goto "bargain"` |
| `accept_bargain` | Bargain → Scene transition | `Goto "sceneRouter"` in bargain handler |
| `retreat` | Bargain → Downtime transition | `Goto "downtime"` in bargain handler |
| `pass_out` | Bargain → Trauma transition | `Goto "trauma"` in bargain handler |
| `choose` | Weighted random selection | Regular effect (not a transition) |

**Key change**: Transition tools become Goto effects in handlers, not standalone tools.

### Phase 3: Implement Handlers

Each LLM node needs:
- **Before handler**: Builds template context from payload + WorldState
- **After handler**: Processes TurnOutput, applies to state, decides next transition

Example pattern:
```haskell
sceneEncounterHandler :: LLMHandler PlayerInput TurnOutput DMSceneEffects es SceneContext
sceneEncounterHandler = LLMBoth
  systemTemplate
  userTemplate
  buildSceneContext      -- before: PlayerInput → SceneContext
  processSceneOutput     -- after: TurnOutput → GotoChoice DMSceneEffects
```

### Phase 4: Handle Special Cases

1. **Trauma trigger check**
   - After applying TurnOutput, check if stress >= 9
   - If so, return `Goto "trauma"` instead of normal next state

2. **Dice selection**
   - `spend_die` becomes a sequence:
     1. Build dice display from pool
     2. `RequestInput` to get player selection
     3. Apply selection to state
     4. If pool now empty → `Goto "bargain"`
     5. Else continue with resolution

3. **Scene compression**
   - After 20+ beats, compress before continuing
   - This is a state effect, not a transition

4. **BetweenScenes phase**
   - Currently separate from dmTurn
   - Becomes the `downtime` node with player choices
   - Returns to `sceneRouter` when ready for new scene

### Phase 5: Wire Up Execution

1. **Create handler record**
   ```haskell
   dmHandlers :: DMGraph (AsHandler DMEffectStack)
   ```

2. **Implement graph runner**
   ```haskell
   runDMGraph :: PlayerInput -> Eff DMEffectStack Response
   runDMGraph input = do
     initialChoice <- determineStartNode input  -- Based on current mood
     dispatchGoto dmHandlers initialChoice
   ```

3. **Entry point routing**
   - Entry receives `PlayerInput`
   - Need to determine current mood from state
   - Route to appropriate scene variant or current phase

## Effect Stack

```haskell
type DMEffectStack =
  '[ State WorldState      -- Game state
   , RequestInput          -- Player choices (dice, text, options)
   , Emit DMEvent          -- Event log for GUI
   , Random                 -- Dice rolls
   , Time                   -- Timestamps
   , Log                    -- Debug logging
   , LLM                    -- Language model calls
   ]
```

## Migration Strategy

### Step 1: Parallel Implementation
- Create `DM/Graph.hs` with new graph structure
- Create `DM/Graph/Types.hs` with payload types
- Create `DM/Graph/Handlers.hs` with handler implementations
- Keep existing `DM/Loop.hs` working

### Step 2: Incremental Migration
- Start with simplest path: Scene → Action → Aftermath → Scene
- Test each transition in isolation
- Add Bargain/Trauma paths
- Add Downtime path

### Step 3: Replace Entry Point
- Update `runDMGame` to use graph execution
- Verify GUI integration still works
- Remove old loop code

## Type Safety Benefits

1. **Compile-time transition validation**
   - Can't `Goto "action"` from Downtime node
   - All targets must exist and accept the payload type

2. **Exhaustive pattern matching**
   - `GotoChoice` forces handling all transition options
   - No forgotten edge cases

3. **Clear data flow**
   - Each transition carries typed payload
   - No implicit state threading for control flow

4. **Testable handlers**
   - Each handler is a pure function
   - Can unit test transitions without running full graph

## Open Questions

1. **Should scene variants be separate nodes or one node with variant payload?**
   - Separate nodes: Cleaner types, but more boilerplate
   - One node: More compact, but variant handling in handler
   - **Recommendation**: Separate nodes for type safety

2. **How to handle the initial mood routing?**
   - Entry could route based on saved mood in WorldState
   - Or always start fresh from SceneRouter
   - **Recommendation**: Add a `resume` logic node that reads state and routes

3. **Where does compression fit?**
   - Not a transition, just state modification
   - Could be an effect in each handler
   - **Recommendation**: Check after each LLM call, before deciding next Goto

4. **How to handle session persistence?**
   - Current: WorldState is saved/loaded
   - Graph: Same, but also need to save "current node"
   - **Recommendation**: Store mood in WorldState, use `resume` node on load

## File Structure

```
tidepool-dm/src/DM/
├── Graph.hs                 # DMGraph record definition
├── Graph/
│   ├── Types.hs             # Payload types (ActionContext, etc.)
│   ├── Context.hs           # Phase-specific context types for templates
│   ├── Templates.hs         # TemplateDef instances
│   ├── Handlers.hs          # Handler implementations
│   ├── Effects.hs           # DMEffectStack, effect interpreters
│   └── Run.hs               # runDMGraph, entry points
├── State.hs                 # (keep) WorldState, PlayerState, etc.
├── Output.hs                # (keep) TurnOutput, applyTurnOutput
└── GUI/                     # (keep) GUI remains unchanged
```

## Success Criteria

1. All existing mood transitions work correctly
2. GUI displays correct state at each phase
3. Dice selection works with RequestInput
4. Trauma triggers correctly on stress >= 9
5. Scene compression still works
6. Session save/load preserves current phase
7. Tests pass for each transition path
