---
name: session-review
description: Analyze a play session transcript to identify storytelling issues, flow/confusion moments, and prompt improvements. Paste session output and get structured feedback.
---

# Session Review

Analyze a DM agent play session for storytelling quality and prompt effectiveness.

## How to Use

1. Paste a session transcript (from agent output)
2. This skill will analyze from multiple perspectives
3. Outputs: bugs, flow moments, confusion moments, prompt fixes

## Analysis Framework

### Perspective 1: QA Tester
Look for mechanical issues:
- Double transitions (engage firing twice)
- Missing state changes (clocks not ticking)
- Wrong tool selection (engage on safe actions)
- Display issues (deltas not shown)

### Perspective 2: The Player
Identify experience issues:
- Moments of confusion ("wait, I killed someone?")
- Walls of text before agency returns
- Missing information for decisions
- Over/under-triggering of mechanics

### Perspective 3: A Narrative Designer
Assess storytelling quality:
- NPC voice distinctiveness
- Consequence clarity (lethal vs non-lethal)
- Pacing (hook → action → consequence → hook)
- Weight of major decisions

## Output Format

```
## Bugs (mechanical)
- BUG-001: [symptom] → [root cause] → [severity]

## Flow Moments (what worked)
- [moment]: [why it worked]

## Confusion Moments (what broke)
- [moment]: [what player expected vs got]

## Prompt Fixes
1. [file]: [specific change]
```

## Using Metacog

Summon these voices for deeper analysis:

```
mcp__metacog__summon:
  who: "A QA tester"
  where: "filing bug reports after playtest"
  lens: "what system did vs should have done"

mcp__metacog__summon:
  who: "The player from this session"
  where: "reflecting afterward"
  lens: "moments of confusion vs flow"

mcp__metacog__summon:
  who: "Keith Johnstone"
  where: "watching improv scene die"
  lens: "what killed spontaneity vs enabled it"
```

## Common Issues to Check

- [ ] Did engage fire appropriately? (opposition present AND aware)
- [ ] Were violence outcomes unambiguous? (lethal vs non-lethal)
- [ ] Did clocks tick when they should?
- [ ] Was aftermath pacing tight? (one paragraph before agency)
- [ ] Did major decisions land with weight?
- [ ] Did downtime requests trigger montage?
