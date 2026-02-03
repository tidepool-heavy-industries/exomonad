# Research Brief: Template Haskell vs. Extism Host Functions in GHC 9.12 WASM

**Date:** 2026-02-03
**Subject:** Build failures when using Template Haskell (`polysemy-plugin` / `makeSem`) in modules containing Extism FFI imports.

## Context
We are building `wasm-guest` (Haskell) for the ExoMonad runtime. The target is `wasm32-wasi` using GHC 9.12.2 (via Nix). The project uses `polysemy` for effects.

## The Anomaly
The user asserts that Template Haskell (TH) support was fixed in GHC 9.12 for WASM. However, we observed the following crash during the build of `ExoMonad.Guest.Effects.UI` when `makeSem` (TH) was enabled:

```text
[ 7 of 23] Compiling ExoMonad.Guest.Effects.UI ...
<no location info>: error:
    loadDLLs failed: JSException "Error: cannot handle import extism:host/env.input_length with kind function
    at DyLD.loadDLLs (file:///nix/store/...-wasm32-wasi-ghc-9.12/lib/dyld.mjs:1197:15)
    ...
```

## Diagnosis
While TH itself *does* work on GHC 9.12 (via an external interpreter, usually running on Node.js), this specific failure is due to **unresolved host symbols**.

1.  **Dependency Chain:** `ExoMonad.Guest.Effects.UI` imports `ExoMonad.Guest.HostCall`.
2.  **FFI Imports:** `HostCall` defines `foreign import ccall "git_get_branch" ...` which resolves to imports from the WASM host environment (specifically `extism:host/env`).
3.  **TH Execution:** When `makeSem` runs, GHC must load and execute the module's bytecode/object code to evaluate the splice.
4.  **Linker Failure:** The GHC WASM interpreter (likely `iserv-proxy` running on `node`) loads the WASM module. It encounters imports for `extism:host/env` functions. The interpreter does **not** provide these functions (it is not the Extism runtime).
5.  **Crash:** The dynamic linker (`DyLD`) throws a `JSException` because it cannot resolve the imports required to run the TH code.

## The Workaround
We bypassed the issue by manually implementing the code that `makeSem` would have generated:
1.  Removed `makeSem ''UI`.
2.  Removed `{-# LANGUAGE TemplateHaskell #-}`.
3.  Manually wrote the smart constructor `showPopup` and the effect interpreter `runUI`.

This pattern was already established in `ExoMonad.Guest.Effects.FileSystem`:
```haskell
-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
```

## Research Questions for Deep Agent
To enable "true" TH support in this environment, we need to answer:

1.  **Mocking Host Functions:** Is there a standard way to provide "stub" or "mock" implementations of foreign imports to the GHC WASM interpreter during compilation?
    *   *Hypothesis:* Can we link against a `cbits` stub file that provides no-op implementations of `extism` functions just for the IServ environment?
2.  **Split Compilation:** Can we structure the code such that the *types* (needed for TH) are in a separate module from the *FFI calls* (which break the interpreter)?
    *   *Experiment:* Move `data UI ...` to `Types.hs` and `runUI` (with FFI) to `Interpreter.hs`. Does `makeSem` in `Types.hs` still trigger the load of `Interpreter.hs`? (Likely not, but `Interpreter` needs the TH-generated code).
3.  **GHC Flags:** Are there GHC flags to tell the WASM linker to ignore missing symbols during TH execution (if the TH code path doesn't actually *call* them)?

## Artifacts
- **File:** `haskell/wasm-guest/src/ExoMonad/Guest/Effects/UI.hs` (Manual implementation)
- **Log:** See build output for `ExoMonad.Guest.Effects.UI` failure.
