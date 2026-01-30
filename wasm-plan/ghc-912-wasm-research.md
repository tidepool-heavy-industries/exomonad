# Research: GHC 9.12 WASM Build Support

## Problem Statement

We want to build our Haskell WASM guest (`haskell/wasm-guest/`) using GHC 9.12 to match our native development environment, but the current `ghc-wasm-meta` flake only provides GHC 9.10.

## Current Setup

### Native Development (working)
- **GHC**: 9.12.2 (via nix flake, `haskellPkgs "ghc912"`)
- **base**: 4.21.0.0
- **cabal.project.freeze**: Locked to GHC 9.12 dependencies

### WASM Build (working, but on 9.10)
- **ghc-wasm-meta**: `gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org`
- **Package**: `wasmPkgs.all_9_10` (GHC 9.10.3.20251220)
- **base**: 4.20.2.0
- **Toolchain**: `wasm32-wasi-ghc`, `wasm32-wasi-cabal`

### flake.nix Configuration
```nix
inputs = {
  ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
};

# WASM shell uses:
wasmPkgs.all_9_10  # GHC 9.10 WASM toolchain
```

## What We Tried

### 1. Direct Build with Project Freeze
```bash
nix develop .#wasm
wasm32-wasi-cabal build wasm-guest
```

**Error:**
```
Error: [Cabal-7107]
Could not resolve dependencies:
[__0] trying: exomonad-actor-0.1.0.0 (user goal)
[__1] next goal: base (dependency of exomonad-actor)
[__1] rejecting: base; 4.20.2.0/installed-inplace, 4.22.0.0, 4.21.1.0
      (constraint from cabal.project.freeze requires ==4.21.0.0)
[__1] rejecting: base-4.21.0.0 (constraint from non-reinstallable package requires installed instance)
```

**Root Cause:** The `cabal.project.freeze` was generated with GHC 9.12 (base 4.21.0.0), but WASM GHC 9.10 has base 4.20.2.0.

### 2. Isolated Build (workaround, working)
Built in a temp directory with minimal cabal.project to avoid freeze file:

```bash
TMPDIR=$(mktemp -d)
cp -r haskell/wasm-guest $TMPDIR/
cp -r haskell/vendor/freer-simple $TMPDIR/

cat > $TMPDIR/cabal.project << 'EOF'
packages: wasm-guest freer-simple
allow-newer: all
EOF

cd $TMPDIR
wasm32-wasi-cabal build wasm-guest
```

**Result:** Successfully builds wasm-guest.wasm (3.2MB)

## Questions for Research

### 1. GHC 9.12 Availability in ghc-wasm-meta
- Is GHC 9.12 available in ghc-wasm-meta? If so, what's the package name?
- Check: `wasmPkgs.all_9_12` or similar?
- What's the latest GHC version available in ghc-wasm-meta?

### 2. If GHC 9.12 Not Available
- What's the timeline for GHC 9.12 WASM support?
- Are there known blockers (tail calls, WASM features, RTS issues)?
- Is there a tracking issue/milestone?

### 3. Architecture: Handling Version Mismatch
If we must use different GHC versions for native (9.12) vs WASM (9.10), what are the options?

**Option A: Separate freeze files**
- `cabal.project.freeze` for native (9.12)
- `cabal.project.freeze.wasm` for WASM (9.10)
- How to make cabal use the right one?

**Option B: Exclude wasm-guest from main project**
- Remove wasm-guest from root `cabal.project`
- Give it its own `cabal.project` in `haskell/wasm-guest/`
- Build it independently

**Option C: No freeze file for WASM**
- Always build WASM in isolated directory (current workaround)
- Reproducibility concerns?

**Option D: Pin both to GHC 9.10**
- Downgrade native to 9.10 to match WASM
- Loses access to 9.12 features

### 4. Nix Integration
- Can we have nix generate a WASM-specific freeze file?
- Should the wasm shell include a pre-build step to copy and configure?
- Example patterns from other Haskell WASM projects?

### 5. Alternative WASM Toolchains
- Are there other sources for GHC WASM besides ghc-wasm-meta?
- GHC upstream WASM backend status for 9.12?

## Current ghc-wasm-meta Packages

From our flake evaluation, these packages are available:
- `wasmPkgs.all_9_10` - GHC 9.10 WASM toolchain (confirmed working)
- Need to check: `wasmPkgs.all_9_12`, `wasmPkgs.all_9_11`?

## Resources to Check

1. **ghc-wasm-meta GitLab**: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
2. **GHC WASM backend status**: https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly
3. **GHC 9.12 release notes**: WASM-related changes
4. **Discourse/Reddit**: "ghc 9.12 wasm" discussions

## Desired Outcome

1. **Ideal**: Build wasm-guest with GHC 9.12 to match native development
2. **Acceptable**: Clean architecture for managing 9.10 WASM builds alongside 9.12 native
3. **Document**: Best practices for multi-GHC-version Haskell projects with WASM targets

## Build Verification

Once GHC 9.12 WASM is working, verify:
```bash
wasm32-wasi-cabal build wasm-guest
wasm-tools print --skeleton wasm-guest.wasm | grep -E 'export|import.*env'

# Expected exports:
# - hs_init
# - handle_mcp_call
# - handle_pre_tool_use

# Expected imports (host functions):
# - git_get_branch, git_get_worktree, git_get_dirty_files, git_get_recent_commits
# - docker_exec, docker_spawn, docker_kill
# - github_list_issues, github_get_issue, github_create_pr, github_list_prs
# - log_info, log_error, emit_event
```
