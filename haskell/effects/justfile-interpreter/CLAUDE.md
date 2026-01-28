# Justfile Interpreter

Implements:
- `Justfile`: Runs recipes via `just` CLI (simple string output).
- `JustExec`: Runs recipes via `docker-ctl` (structured JSON output).

## Build and Test
- Build: `cabal build exomonad-justfile-interpreter`
- Test: `cabal test exomonad-justfile-interpreter`

## Dependencies
- `just` (for `Justfile` effect)
- `docker-ctl` (for `JustExec` effect)
