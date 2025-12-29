# Tidying App Architecture Diagrams

Mermaid diagrams documenting the complete architecture of the Tidying agent.

## Diagram Index

| File | Description |
|------|-------------|
| [state-machine.md](state-machine.md) | Phase states, transitions, PhaseData types |
| [ooda-loop.md](ooda-loop.md) | OODA cycle with type transformations |
| [data-flow.md](data-flow.md) | Type pipeline from input to response |
| [effect-stack.md](effect-stack.md) | Effect interpretation order |
| [gui-communication.md](gui-communication.md) | Two-thread architecture, TVar/MVar sync |
| [tool-execution.md](tool-execution.md) | Tool → QuestionHandler → GUI flow |

## Viewing the Diagrams

Mermaid diagrams render natively in:
- **GitHub** - Markdown preview shows diagrams
- **VS Code** - With Mermaid extension
- **GitLab** - Markdown preview
- **Obsidian** - Native support
- **mdBook** - With mermaid preprocessor

## Quick Reference

### Phases
```
Surveying → Sorting → Splitting → Refining → DecisionSupport
```

### OODA Cycle
```
OBSERVE (photos) → ORIENT (extract) → DECIDE (route) → ACT (respond)
```

### Effect Stack (outermost first)
```
IOE → Time → Random → Emit → State → ChatHistory → Log → RequestInput → LLM → QuestionUI
```

### Key Files
- `Loop.hs` - OODA implementation
- `Decide.hs` - Pure routing logic
- `State.hs` - PhaseData, SessionState
- `GUI/Runner.hs` - Effect stack wiring
