# Dead Code Audit Report
Date: 2026-02-15

## Summary
This report identifies dead code, unused imports, and potential cleanup targets in the Rust codebase.

## Compiler Warnings
`cargo check` (workspace) reported no warnings.

## Remediation Status

The following unused public functions and dead code blocks were identified and **removed** in this PR:

- `exomonad-core::services::secrets::Secrets::anthropic_api_key`
  - Reason: Superseded by environment variable export in `Secrets::load()`.
  - Status: **Removed**

- `exomonad-core::services::filesystem::FileSystemService::from_cwd`
  - Reason: Unused convenience constructor.
  - Status: **Removed**

- `exomonad-core::domain::Topology::from_proto`
  - Reason: Unused conversion from protobuf integer.
  - Status: **Removed**

- `exomonad-core::domain::SessionId::from_str_unchecked`
  - Reason: Unused test helper.
  - Status: **Removed**

- `exomonad-core::domain::AbsolutePath::into_path_buf`
  - Reason: Unused conversion.
  - Status: **Removed**

- `exomonad-core::ffi::FFIResult::simple_error`
  - Reason: Unused helper constructor.
  - Status: **Removed**

- `exomonad-core::RuntimeBuilder::with_effect_handler_arc`
  - Reason: Unused builder method.
  - Status: **Removed**

- `exomonad-core::RuntimeBuilder::into_registry`
  - Reason: Unused builder terminator.
  - Status: **Removed**

- `#[allow(dead_code)]` in `rust/exomonad-core/src/services/github_poller.rs`
  - Removed `branch_name` field from `PRState`.
  - Status: **Removed**

- `#[allow(dead_code)]` in `rust/exomonad/src/pid.rs`
  - Renamed `file` to `_file` to indicate intentional unused (RAII guard).
    - Status: **Fixed**
  
  - `// TODO: Return actual worktree path` in `rust/exomonad-core/src/services/agent_control.rs`
    - Reason: Feature improvement.
    - Status: **Fixed**
  
  ## TODOs and FIXMEs (Remaining)
  - `rust/exomonad-core/src/plugin_manager.rs`: `// TODO: Fix for Extism 1.13`
  - `rust/exomonad-core/src/effects/mod.rs`: `//!         todo!()` (Documentation example)
  