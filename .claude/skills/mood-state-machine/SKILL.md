---
name: mood-state-machine
description: Use when working with DM moods, state transitions between Scene/Action/Aftermath/Downtime, or debugging why mood changes aren't happening correctly.
---

# Mood State Machine

The DM operates as a state machine with distinct moods, each with its own template and available tools.

## States (Moods)

```
┌─────────┐    engage     ┌─────────┐   resolve   ┌───────────┐
│  SCENE  │──────────────▶│ ACTION  │────────────▶│ AFTERMATH │
└─────────┘               └─────────┘             └───────────┘
     ▲                                                   │
     │                      accept                       │
     └───────────────────────────────────────────────────┘
```

## Mood Types

```haskell
data DMMood
  = MoodScene SceneVariant      -- Exploration, NPC encounters
  | MoodAction ActionVariant (Maybe ActionDomain)  -- Risky action resolution
  | MoodAftermath AftermathVariant  -- Consequences landing
  | MoodDowntime DowntimeVariant    -- Recovery, projects
  | MoodTrauma TraumaVariant        -- Special handling
```

## Transition Tools

| Tool | From | To | Purpose |
|------|------|-----|---------|
| `engage` | Scene | Action | Player commits to risky action |
| `resolve` | Action | Aftermath | Dice resolved, consequences manifest |
| `accept` | Aftermath | Scene | Player accepts situation, returns to exploration |

## How Transitions Work

1. LLM calls transition tool (e.g., `engage`)
2. Tool modifies `mood` field in `WorldState`
3. Tool returns via dispatcher
4. Dispatcher detects mood change, returns `ToolBreak`
5. `runMoodAwareTurn` catches break, recurses with new mood
6. New iteration renders different template for new mood

## Template Selection

```haskell
renderForMood :: DMMood -> DMContext -> Text
renderForMood mood ctx = case mood of
  MoodScene _     -> render sceneTemplate ctx
  MoodAction _ _  -> render actionTemplate ctx
  MoodAftermath _ -> render aftermathTemplate ctx
  MoodDowntime _  -> render downtimeTemplate ctx
```

## Key Insight

The LLM doesn't know it's being restarted. It just sees a new system prompt with different instructions. The "conversation" continues but the rules change.

## OODA Mapping (Future)

The mood state machine is essentially OODA:
- Scene = Observe + Orient
- Action = Decide
- Aftermath = Act
- (then back to Scene = new Observe)

Future work: make stages more granular, allow semantic routing between them.
