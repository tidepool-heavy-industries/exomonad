# Dead Code Audit Report
Date: 2026-02-15

## Summary
This report identifies dead code, unused imports, and potential cleanup targets in the Rust codebase.

## Compiler Warnings
`cargo check` (workspace) reported no warnings.

## Unused Public Functions
The following public functions appear to be unused (found 1 occurrence, which is the definition):

- `exomonad-core::services::secrets::Secrets::anthropic_api_key`
  - Reason: Superseded by environment variable export in `Secrets::load()`.
  - Location: `rust/exomonad-core/src/services/secrets.rs`

- `exomonad-core::services::filesystem::FileSystemService::from_cwd`
  - Reason: Unused convenience constructor; `new()` is used with explicit paths.
  - Location: `rust/exomonad-core/src/services/filesystem.rs`

- `exomonad-core::domain::Topology::from_proto`
  - Reason: Unused conversion from protobuf integer.
  - Location: `rust/exomonad-core/src/services/agent_control.rs`

- `exomonad-core::domain::SessionId::from_str_unchecked`
  - Reason: Test helper unused in current test suite.
  - Location: `rust/exomonad-core/src/domain.rs`

- `exomonad-core::domain::AbsolutePath::into_path_buf`
  - Reason: Unused conversion (trait `Into<PathBuf>` or `AsRef` likely used instead).
  - Location: `rust/exomonad-core/src/domain.rs`

- `exomonad-core::ffi::FFIResult::simple_error`
  - Reason: Unused helper constructor.
  - Location: `rust/exomonad-core/src/ffi.rs`

- `exomonad-core::RuntimeBuilder::with_effect_handler_arc`
  - Reason: Unused builder method; `with_effect_handler` (owned) or `with_handlers` (boxed) are used.
  - Location: `rust/exomonad-core/src/lib.rs`

- `exomonad-core::RuntimeBuilder::into_registry`
  - Reason: Unused builder terminator; `build()` is used to create the runtime.
  - Location: `rust/exomonad-core/src/lib.rs`

## Dead Code Annotations
The following code is explicitly marked as dead code and should be reviewed for removal:

- `rust/exomonad-core/src/services/github_poller.rs` (L26)
- `rust/exomonad/src/pid.rs` (L220)

## TODOs and FIXMEs
- `rust/exomonad-core/src/plugin_manager.rs`: `// TODO: Fix for Extism 1.13`
- `rust/exomonad-core/src/effects/mod.rs`: `//!         todo!()` (Documentation example)
- `rust/exomonad-core/src/services/agent_control.rs`: `// TODO: Return actual worktree path if possible...`

## Recommendations
1. Remove `Secrets::anthropic_api_key` as it is redundant.
2. Remove unused `RuntimeBuilder` methods if the API stability is not a concern (internal crate).
3. Investigate `Topology::from_proto` usage; if strictly for future use, keep it, otherwise remove.
4. Review `#[allow(dead_code)]` blocks to see if the code is truly needed.
