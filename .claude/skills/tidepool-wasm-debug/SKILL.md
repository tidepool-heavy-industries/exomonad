---
name: exomonad-wasm-debug
description: Use when debugging WASM builds, effect serialization issues, or stale cabal cache problems.
---

# WASM Build Debugging

Debug issues with WASM compilation, effect protocol, or cabal package resolution.

## Stale Effects (Wrong Effect Type in WASM)

**Symptom**: TypeScript sees `LlmComplete` when expecting `LlmCall`, or other outdated effect types.

**Root cause**: cabal cache not picking up new package versions.

### Verification Steps

1. **Check package resolution**:
   ```bash
   cd exomonad
   grep -A 5 "exomonad-wasm" cabal.project
   # Should show: packages: ../exomonad/exomonad-wasm
   ```

2. **Verify source in plan.json**:
   ```bash
   cabal build exomonad-wasm --dry-run
   cat dist-newstyle/cache/plan.json | jq '.["install-plan"][] | select(.["pkg-name"] == "exomonad-wasm") | .["pkg-src"]'
   # Should show: {"type":"local","path":"..."}
   ```

3. **Nuclear clean**:
   ```bash
   rm -rf dist-newstyle ~/.cabal/store  # Nuclear option
   cabal update
   cabal build exomonad-wasm
   ```

## Effect Serialization Check

Verify effect types match between Haskell and TypeScript:

```bash
# Check Haskell side
rg "EffLlmCall" exomonad-wasm/src/ExoMonad/Wasm/

# Check TypeScript side
rg "LlmCall" deploy/src/protocol.ts
```

**Key files**:
- `exomonad-wasm/src/ExoMonad/Wasm/Effect.hs` - Smart constructors (`llmCall`, `llmComplete`)
- `exomonad-wasm/src/ExoMonad/Wasm/WireTypes.hs` - Serialization (`EffLlmCall`, `EffLlmComplete`)
- `deploy/src/protocol.ts` - TypeScript types

## WASM Build Location

After successful build:
```bash
find dist-newstyle -name "*.wasm" -type f
# Example: dist-newstyle/build/wasm32-wasi/ghc-9.10.1/exomonad-wasm-0.1.0.0/x/exomonad-wasm/build/exomonad-wasm/exomonad-wasm.wasm
```

## Common Issues

1. **Stale package cache** - `cabal clean` or nuke `dist-newstyle`
2. **Wrong package source** - Check `plan.json` for `"type":"local"`
3. **Field name mismatch** - Check ToJSON instance matches protocol.ts
4. **Missing CPP guards** - WASM-specific code needs `#if defined(wasm32_HOST_ARCH)`
