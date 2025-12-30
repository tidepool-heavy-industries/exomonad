# User Journeys

Player experience flows.

## Happy Path: Player Turn

Standard flow through Scene -> Action -> Scene (dice auto-returns to Scene).

```mermaid
journey
    title Player Turn Flow
    section Scene
      Player describes action: 5: Player
      DM narrates response: 5: LLM
      DM offers choices: 4: LLM
    section Action
      DM calls engage: 3: LLM
      Player sees dice pool: 5: Player
      Player picks die: 5: Player
      Costs applied: 3: System
    section Resolution
      Outcome narrative revealed: 5: LLM
      Auto-return to Scene: 4: System
    section Trauma (if stress=9)
      Breaking point: 1: System
      DM assigns trauma: 2: LLM
      Return to scene: 3: LLM
```

## Bargain Path: Out of Dice

When the dice pool is empty during Action mood.

```mermaid
journey
    title Bargain Flow
    section Action
      Player commits to action: 4: Player
      DM calls engage: 3: LLM
      No dice available: 1: System
    section Bargain
      City offers dark deal: 2: LLM
      Player considers options: 3: Player
    section Resolution
      Accept bargain - continue: 3: Player
      OR Retreat - end scene: 2: Player
      OR Pass out - wake elsewhere: 1: Player
```

## Session Arc

Full session from character creation to end.

```mermaid
journey
    title Session Arc
    section Setup
      Create character: 5: Player
      Generate scenario: 4: LLM
      Enter first scene: 5: Player
    section Play Loop
      Explore and act: 5: Player
      Face consequences: 3: LLM
      Manage resources: 4: Player
      Scene compresses at 20 beats: 3: System
    section Between Scenes
      Choose downtime activity: 4: Player
      Lay low OR recover OR new scene: 4: Player
    section End
      Player quits: 5: Player
      Session summary: 4: System
```

## Dice Selection Experience

What the player sees and decides during Action mood.

```mermaid
journey
    title Dice Selection
    section Setup
      See situation description: 4: Player
      See position (Risky): 3: Player
    section Choice
      View all dice with hints: 5: Player
      See costs per die: 4: Player
      Weigh opportunity cost: 3: Player
    section Commit
      Select die: 5: Player
      Watch costs apply: 3: System
      Read outcome narrative: 5: Player
```

### Dice Display Example

```
SITUATION: Dodging the Bluecoat's blade
POSITION: Risky

Your dice pool:
┌─────────────────────────────────────────────────┐
│ [4]  +1 stress  │  "Scrape through, bruised"    │
├─────────────────────────────────────────────────┤
│ [2]  +2 stress  │  "Barely, they saw your face" │
│      +1 heat    │                               │
├─────────────────────────────────────────────────┤
│ [6]  +0 stress  │  "Clean break"                │
└─────────────────────────────────────────────────┘
```

The player must choose which future to spend. High dice give good outcomes but cost opportunity for future actions.
