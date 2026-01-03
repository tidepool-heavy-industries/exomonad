# tidepool-dm - Blades in the Dark DM Agent

A Blades in the Dark-inspired dungeon master agent demonstrating Tidepool's LLM agent framework.

## What This Is

An example agent showcasing:
- **Typed state**: WorldState with factions, clocks, NPCs, locations
- **Delta-based mutations**: LLM outputs deltas ("+2 stress") not absolute values
- **FitD dice mechanics**: Position/Effect, precarity scaling
- **Threepenny-gui**: Browser-based UI with polling pattern

## Status

**Stubbed** - Compiles successfully but core game loop has placeholder implementations.

## Key Files

| File | Purpose |
|------|---------|
| `State.hs` | WorldState, PlayerState, FitD dice mechanics |
| `Output.hs` | TurnOutput with delta fields, applyTurnOutput |
| `Tools.hs` | ThinkAsDM, SpeakAsNPC, AskPlayer, Choose |
| `Context.hs` | DMContext, DiceContext, Precarity |
| `Templates.hs` | dmTurnTemplate, compressionTemplate |
| `Loop.hs` | dmTurn, handleDiceSelection, runDMGame |
| `GUI/` | Noir-themed browser GUI (threepenny-gui) |

## Running

```bash
# CLI (hits TODO errors)
cabal run tidepool-dm

# GUI (opens browser at localhost:8023)
cabal run tidepool-dm-gui
```

## Architecture

See root `CLAUDE.md` for full Tidepool architecture. This package is a concrete agent implementation demonstrating the framework patterns.
