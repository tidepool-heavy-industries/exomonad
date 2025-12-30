# Tidying Agent Architecture Diagrams

Mermaid diagrams documenting the Mode-based architecture of the Tidying agent.

## Diagram Index

| File | Description |
|------|-------------|
| [state-machine.md](state-machine.md) | Mode states, transitions, mode-specific data |
| [turn-loop.md](turn-loop.md) | Core loop: Mode → Template → LLM → Output → Update |
| [data-flow.md](data-flow.md) | Type pipeline, Mode sum type, output schemas |
| [tool-execution.md](tool-execution.md) | Tool dispatch, ToolBreak transitions |
| [effect-stack.md](effect-stack.md) | Effect interpretation order |
| [gui-communication.md](gui-communication.md) | Two-thread architecture, TVar/MVar sync |

## Viewing the Diagrams

Mermaid diagrams render natively in:
- **GitHub** - Markdown preview shows diagrams
- **VS Code** - With Mermaid extension
- **GitLab** - Markdown preview
- **Obsidian** - Native support

## Quick Reference

### Modes (LLM navigates via tools)

| Mode | Persona | Data |
|------|---------|------|
| `Surveying` | Curious, orienting | (empty) |
| `Sorting` | Terse, directive | currentItem, itemLocation |
| `Clarifying` | Patient, descriptive | item, photoContext, reason |
| `DecisionSupport` | Gentle, reframing | stuckItem |
| `WindingDown` | Warm, factual | (summary fields) |

### Core Loop

```
Mode (sum type) → Template → LLM → Structured Output → Update Mode Data
                                         ↓
                              Transition tool called?
                                         ↓
                              Yes → New Mode with initial data
```

### Transition Tools

| Tool | From | To | Creates |
|------|------|-----|---------|
| `begin_sorting` | Surveying | Sorting | SortingData {} |
| `need_to_clarify` | Sorting | Clarifying | ClarifyingData {item, context, reason} |
| `user_seems_stuck` | Sorting | DecisionSupport | DecisionSupportData {item} |
| `time_to_wrap` | Sorting | WindingDown | WindingDownData {} |
| `resume_sorting` | Clarifying/DecisionSupport | Sorting | SortingData {} |

### Effect Stack (outermost first)

```
IOE → Time → Random → Emit → State → ChatHistory → Log → RequestInput → LLM → QuestionUI
```

### Key Files

- `State.hs` - Mode sum type, SessionState
- `Loop.hs` - Core turn loop
- `Tools.hs` - Mode-specific tool sets, transition tools
- `GUI/Runner.hs` - Effect stack wiring
