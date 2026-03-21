# Copilot Code Review Instructions

## Architecture Rule

**All MCP tool logic lives in Haskell WASM. Rust handles I/O only.**

If a PR adds tool schemas, argument parsing, or dispatch logic to Rust code, flag it. Tools belong in `haskell/wasm-guest/src/ExoMonad/Guest/Tools/`. Rust only adds effect handlers (new I/O capabilities) in `rust/exomonad-core/src/handlers/`.

## Language-Specific Guidance

### Haskell (haskell/wasm-guest/, .exo/roles/, .exo/lib/)

**Effect System (freer-simple)**
- WASM guest code must NOT do I/O — it yields typed effects via `runEffect @EffectType`
- Effects use protobuf-encoded requests/responses across the WASM boundary
- `suspendEffect_` is the low-level yield; smart constructors in `ExoMonad.Effects.*` are preferred

**Tool Definitions**
- Tools use the `mode :- Tool` record pattern (`AsSchema` for schemas, `AsHandler` for dispatch)
- Tool handlers return `Eff '[EffectYield] ToolResult`
- New tools must be wired into role configs in `.exo/roles/devswarm/`

**Proto Field Naming**
- Generated Haskell types use `messageName` + `FieldName` convention
- Example: `GetBranchRequest` field `working_dir` → `getBranchRequestWorkingDir`
- Use `Data.Text.Lazy.Text` for proto strings, `Data.Vector.Vector` for repeated fields

**LANGUAGE Pragmas**
- Closing delimiter is `#-}` not `#}` — verify after any edit to pragma blocks

### Rust (rust/)

**Effect Handlers**
- Implement auto-generated traits from proto service definitions
- Use `EffectError` variants (`not_found`, `invalid_input`, `custom`) — not `anyhow`
- Use `ResultExt::effect_err(namespace)` for error conversion
- Register handlers in `EffectRegistry` by namespace prefix

**Logging**
- Log before subprocess/API calls (command + key params)
- Log after (exit code, result summary)
- Log on error (stderr, enough context to debug without reproducing)

### Proto (proto/)

- Proto files are the single source of truth for FFI types
- `service` blocks drive Rust trait codegen (`build.rs`)
- Changes require both `just proto-gen-haskell` AND `cargo build -p exomonad-proto`

## Review Focus

1. **Effect boundary** — Haskell WASM yields effects, Rust executes I/O. New I/O in Rust, new logic in Haskell, proto types bridge them. WASM must not escape its effect sandbox
2. **Logging** — Subprocess calls must log before/after/error per CLAUDE.md policy
3. **No dead code** — No `todo!()`, placeholder variants, or half-done heuristics

## Less Important

- Style/formatting (hlint and clippy handle this)
- Documentation coverage (intentionally minimal in code)
- Test coverage suggestions (tests are for critical paths only)
