# TODO

## CI Container Cache - Push to GHCR

The Docker container with pre-built Haskell dependencies is built locally but needs to be pushed to GitHub Container Registry to enable fast CI builds.

### Current Status
- ✅ Container built locally: `ghcr.io/inanna-malick/tidepool/ci-cache:latest`
- ✅ All Haskell dependencies cached (~3.5 min build time)
- ⏳ **Needs push to GHCR** to enable CI usage

### Steps to Complete

1. **Create GitHub Personal Access Token**
   - Go to https://github.com/settings/tokens/new
   - Name: "GHCR Package Write"
   - Scopes: `write:packages`, `read:packages`
   - Generate and copy token

2. **Push Container**
   ```bash
   # Login to GHCR
   echo "YOUR_TOKEN" | docker login ghcr.io -u inanna-malick --password-stdin

   # Push both tags
   docker push ghcr.io/inanna-malick/tidepool/ci-cache:latest
   docker push ghcr.io/inanna-malick/tidepool/ci-cache:e678e09
   ```

3. **Verify CI Uses Container**
   - Next PR or push to main will automatically use cached container
   - Build time should drop from ~15 min → ~30 sec (30x speedup)

### Impact
- **Before**: ~15 minutes per CI run (building 200+ Haskell packages)
- **After**: ~30 seconds per CI run (dependencies pre-built)
- **Savings**: ~1,000 → 30,000 minutes/month (well within free tier)

### Container Rebuild
Container auto-rebuilds on dependency changes via `.github/workflows/build-container.yml` when:
- `**/*.cabal` changes
- `cabal.project` changes
- `.github/Dockerfile.ci` changes

### Alternative: Local Rebuild
If needed, rebuild and push manually:
```bash
./scripts/build-and-push-container.sh
```
