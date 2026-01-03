---
name: tidepool-wasm-debug
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
   cd shoal-automat
   grep -A 5 "tidepool-wasm" cabal.project
   # Should show: packages: ../tidepool/tidepool-wasm
   ```

2. **Verify source in plan.json**:
   ```bash
   cabal build tidepool-wasm --dry-run
   cat dist-newstyle/cache/plan.json | jq '.["install-plan"][] | select(.["pkg-name"] == "tidepool-wasm") | .["pkg-src"]'
   # Should show: {"type":"local","path":"..."}
   ```

3. **Nuclear clean**:
   ```bash
   rm -rf dist-newstyle ~/.cabal/store  # Nuclear option
   cabal update
   cabal build tidepool-wasm
   ```

## Effect Serialization Check

Verify effect types match between Haskell and TypeScript:

```bash
# Check Haskell side
rg "EffLlmCall" tidepool-wasm/src/Tidepool/Wasm/

# Check TypeScript side
rg "LlmCall" deploy/src/protocol.ts
```

**Key files**:
- `tidepool-wasm/src/Tidepool/Wasm/Effect.hs` - Smart constructors (`llmCall`, `llmComplete`)
- `tidepool-wasm/src/Tidepool/Wasm/WireTypes.hs` - Serialization (`EffLlmCall`, `EffLlmComplete`)
- `deploy/src/protocol.ts` - TypeScript types

## WASM Build Location

After successful build:
```bash
find dist-newstyle -name "*.wasm" -type f
# Example: dist-newstyle/build/wasm32-wasi/ghc-9.10.1/tidepool-wasm-0.1.0.0/x/tidepool-wasm/build/tidepool-wasm/tidepool-wasm.wasm
```

## Common Issues

1. **Stale package cache** - `cabal clean` or nuke `dist-newstyle`
2. **Wrong package source** - Check `plan.json` for `"type":"local"`
3. **Field name mismatch** - Check ToJSON instance matches protocol.ts
4. **Missing CPP guards** - WASM-specific code needs `#if defined(wasm32_HOST_ARCH)`
