# DM System Diagrams

Visual documentation for the DM agent architecture. Each file covers one diagram type.

| File | Contents |
|------|----------|
| [state-machines.md](state-machines.md) | GamePhase (outer lifecycle), DMMood (inner turn states) |
| [sequences.md](sequences.md) | dmTurn flow, dice selection handshake |
| [flowcharts.md](flowcharts.md) | Tool dispatch, clock consequences, compression, events |
| [tool-matrix.md](tool-matrix.md) | Which tools available in which mood |
| [user-journeys.md](user-journeys.md) | Player experience flow |
| [context-pipeline.md](context-pipeline.md) | **DMContext assembly, precarity, mood variants, output schemas** |

## Code References

- State types: `src/DM/State.hs`
- Turn loop: `src/DM/Loop.hs`
- Tools: `src/DM/Tools.hs`
- **Context building: `src/DM/Context.hs`**
- **Template selection: `src/DM/Templates.hs`**
- Templates: `templates/*/main.jinja`
- Full prose docs: `src/DM/CLAUDE.md`
