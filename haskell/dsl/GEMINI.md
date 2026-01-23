# Tidepool DSL: Gemini Intelligence

This directory contains the core Graph DSL and teaching infrastructure for Tidepool LLM orchestration.

## Project Overview

The Tidepool DSL is a Haskell-based system for building type-safe, compile-time validated state machine graphs. It allows for declarative definition of LLM agent workflows where transitions, schemas, and effects are verified at compile time.

### Core Packages

- **`core/` (`tidepool-core`)**: The heart of the system.
    - **Graph DSL**: Type-level graph definitions using `mode :- NodeDef` pattern.
    - **Extensible Effects**: Built on `freer-simple`, enabling IO-blind business logic.
    - **Structured Output**: Robust JSON schema generation and parsing for LLM responses.
    - **Interpret**: Inductive, type-safe graph dispatch via `OneOf` sum types.
- **`teaching/` (`tidepool-teaching`)**: Knowledge distillation infrastructure.
    - Intercepts LLM calls during production-like workflows.
    - Records "Teaching Turns" (Haiku tool use) to generate training data for models like FunctionGemma.

## Building and Running

The project uses standard Haskell `cabal` tooling.

### Common Commands

- **Build all**: `cabal build all`
- **Run core tests**: `cabal test tidepool-core:graph-validation-tests`
- **Run teaching tests**: `cabal test tidepool-teaching:tidepool-teaching-test`
- **Run teaching harness**: `cabal run teaching-harness` (requires `TEACHING_ENABLED=true` and `ANTHROPIC_API_KEY`)

### Platform Targets

- **Native**: Full features, including Claude Code subprocesses and native-only effects.
- **WASM**: Restricted subset for execution in browser/edge environments. Excludes GHCi, LSP, and certain integration effects.

## Development Conventions

### Architectural Pillars

1.  **Extensible Effect Manifold**: All business logic resides in the `Eff` monad. Avoid `IO` in core logic; use interpreters at the application edge.
2.  **Servant-Patterned Graphs**: Define graphs as records using the `mode :- nodeDef` pattern. This allows a single record to function as a type-level spec (`AsGraph`) or a handler map (`AsHandler`).
3.  **Inductive Dispatch**: Transitions use the `OneOf` GADT and `GotoChoice`. Dispatch is performed via recursive typeclass matching, ensuring no `Dynamic` or `unsafeCoerce` is used for state transitions.
4.  **Proof-Carrying I/O**: Use `HasJSONSchema` and `StructuredOutput` (for Anthropic compatibility) to ensure LLM interactions are strictly typed and valid according to the schema.
5.  **Compile-Time Guardrails**: The DSL uses custom `TypeError` constraints to catch graph errors (unreachable nodes, dead ends, invalid targets) during compilation.

### Adding a New Graph Node

1.  Define the node in your graph record (e.g., `myNode :: mode :- LLMNode :@ Input A :@ Template T :@ Schema B`).
2.  Add a handler to your `AsHandler` record.
3.  Ensure the handler returns a `GotoChoice` that transitions to a valid next node or `Exit`.

### Key Files

- `core/src/Tidepool/Graph/Types.hs`: DSL syntax and annotations.
- `core/src/Tidepool/Graph/Interpret.hs`: Logic for running and dispatching graphs.
- `core/src/Tidepool/Effect/Types.hs`: Core effect definitions (LLM, Log, State, etc.).
- `teaching/src/Tidepool/Teaching/LLM.hs`: Teaching interpreter logic.

## Training Infrastructure

When `TEACHING_ENABLED=true` is set, the system can record training data into `.tidepool/training/session-{uuid}/`.
- `anthropic.jsonl`: Raw captured turns.
- `metadata.json`: Session context.
