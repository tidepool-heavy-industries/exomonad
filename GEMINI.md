# ExoMonad — Gemini Agent Rules

You are a Gemini agent working on the ExoMonad codebase. Follow these rules strictly.

## Critical: Non-Interactive Commands

You are running headless. Any command that launches a TUI, pager, or interactive prompt will hang forever.

```bash
# jj: ALWAYS use --no-pager
jj --no-pager log
jj --no-pager status
jj --no-pager diff
jj --no-pager bookmark list

# git: disable pager
git --no-pager log
git --no-pager diff

# NEVER run these without --no-pager — they will hang:
# jj log, jj status, jj diff, git log, git diff
```

## Git Hygiene

- **Always `git add` specific files by name.** Never use `git add .` or `git add -A`.
- **Never commit files under `target/` or build artifact directories.**
- Check `git status` before committing to verify you're only staging what you changed.

## Haskell WASM

- **LANGUAGE pragmas**: The closing delimiter is `#-}` not `#}`. Triple-check after editing .hs files.
- **No trailing whitespace** on lines you edit.
- **Build with**: `just wasm-all` (inside nix develop shell)

## Rust

- **Build with**: `cargo check --workspace` or `cargo test --workspace` (inside nix develop shell)
- **No new dependencies** unless explicitly told to add them.

## Completion Protocol

When your task is done:
1. Commit your changes (`git add <specific files> && git commit -m "..."`)
2. File a PR (`file_pr` MCP tool or `gh pr create`)
3. Call `notify_parent` to signal completion

## Architecture

Read `CLAUDE.md` for full architecture docs. Key points:
- All MCP tool logic lives in Haskell WASM (`haskell/wasm-guest/src/ExoMonad/Guest/Tools/`)
- Rust is the I/O runtime — it executes effects, never defines tool schemas
- Proto files define the FFI boundary between Haskell and Rust
