# CI Container Caching Strategy

## Problem

GitHub Actions minutes were being exhausted because every CI run had to:
1. Download and build all Haskell dependencies from scratch (~200+ packages)
2. Set up Nix environment for WASM builds
3. Install system tools (just, hlint, etc.)

Even with GitHub Actions cache, cache misses and rebuilds were expensive.

## Solution

Pre-build a Docker container with all dependencies and push it to GitHub Container Registry (GHCR). CI jobs pull this cached container and run builds in ~30 seconds instead of ~15+ minutes.

## How It Works

### 1. Build Container (`build-container.yml`)

**Manual trigger only** (`workflow_dispatch`) to avoid burning GitHub Actions minutes on every dependency change. Rebuild when you add significant new dependencies.

Workflow:
1. Uses multi-stage Dockerfile to build all dependencies
2. Pushes container to `ghcr.io/<repo>/ci-cache:latest`
3. Uses Docker layer caching for incremental builds

### 2. CI Workflow (`ci.yml`)

Uses the pre-built container:
```yaml
container:
  image: ghcr.io/${{ github.repository }}/ci-cache:latest
```

Benefits:
- All Haskell dependencies pre-built in container
- System tools (just, hlint, node) already installed
- Only needs to build your actual code
- Builds complete in ~30 seconds vs ~15+ minutes

## First-Time Setup

On a new fork or repo:

1. **Manually trigger container build** (required before CI works):
   - Go to Actions → "Build CI Container" → "Run workflow"
   - Or push a change to any `*.cabal` file

2. **Verify container exists**:
   - Check https://github.com/<user>/<repo>/pkgs/container/ci-cache
   - Should see a `latest` tag

3. **CI will now use cached container**:
   - All subsequent CI runs will be fast
   - Manually rebuild container when adding dependencies

## Maintenance

### When to Rebuild Container

Rebuild the container manually when you:
- Add new dependencies to any `*.cabal` file
- Change `cabal.project` (project config)
- Modify `.github/Dockerfile.ci` (container definition)

To trigger a rebuild:
```bash
# Via GitHub web UI
Actions → Build CI Container → Run workflow
```

### Troubleshooting

**CI fails with "container not found"**:
- Run the "Build CI Container" workflow manually first
- Or push a change to trigger container build

**Dependencies not found in container**:
- Container may be stale
- Trigger rebuild manually or push cabal file change

**Container build fails**:
- Check if base `haskell:9.6` image is accessible
- Verify cabal files are valid
- Check build logs in Actions

## Cost Savings

**Before** (per CI run):
- ~15 minutes build time
- ~15 minutes × 2000 runners = 30,000 minutes/month
- Exhausted free tier (2,000 minutes)

**After** (per CI run):
- ~30 seconds build time
- ~0.5 minutes × 2000 runners = 1,000 minutes/month
- Well within free tier

**Container builds** (only when dependencies change):
- ~10 minutes per rebuild
- ~2-3 rebuilds per month = 20-30 minutes/month
- Net savings: ~29,000 minutes/month

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Dependency Changes (*.cabal, cabal.project)                │
│  ↓                                                           │
│  build-container.yml triggers                               │
│  ↓                                                           │
│  Dockerfile.ci builds container with all deps               │
│  ↓                                                           │
│  Push to ghcr.io/<repo>/ci-cache:latest                     │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  Pull Request / Push to main                                │
│  ↓                                                           │
│  ci.yml triggers                                            │
│  ↓                                                           │
│  Pull cached container (ghcr.io/<repo>/ci-cache:latest)    │
│  ↓                                                           │
│  Build only exomonad code (~30s)                            │
│  ↓                                                           │
│  Run tests                                                  │
└─────────────────────────────────────────────────────────────┘
```

## Files

- `.github/Dockerfile.ci` - Multi-stage Dockerfile that pre-builds deps
- `.github/workflows/build-container.yml` - Builds and pushes container
- `.github/workflows/ci.yml` - Uses cached container for fast builds
