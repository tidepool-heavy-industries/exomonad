#!/usr/bin/env bash
# Build the CI container locally and push to GitHub Container Registry
# Requires: docker, gh CLI for authentication

set -euo pipefail

REPO="${GITHUB_REPOSITORY:-$(git remote get-url origin | sed 's/.*github.com[:/]\(.*\)\.git/\1/')}"
IMAGE="ghcr.io/${REPO}/ci-cache"

echo "=== Building CI container for ${REPO} ==="
echo ""

# Build the container
echo "Building container..."
docker build \
  -t "${IMAGE}:latest" \
  -t "${IMAGE}:$(git rev-parse --short HEAD)" \
  -f .github/Dockerfile.ci \
  .

echo ""
echo "=== Container built successfully ==="
echo ""
echo "Tagged as:"
echo "  - ${IMAGE}:latest"
echo "  - ${IMAGE}:$(git rev-parse --short HEAD)"
echo ""

# Login to GitHub Container Registry
echo "Logging in to GitHub Container Registry..."
echo "You'll need a GitHub Personal Access Token with 'write:packages' permission"
echo ""
read -p "Continue with push? (y/N) " -n 1 -r
echo

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Skipping push. Container is built locally for testing."
  exit 0
fi

# Use gh CLI if available, otherwise prompt for token
if command -v gh &> /dev/null; then
  echo "Using gh CLI for authentication..."
  echo "$(gh auth token)" | docker login ghcr.io -u "$(gh api user -q .login)" --password-stdin
else
  echo "gh CLI not found. Please enter your GitHub username and Personal Access Token:"
  docker login ghcr.io
fi

# Push both tags
echo ""
echo "Pushing container to registry..."
docker push "${IMAGE}:latest"
docker push "${IMAGE}:$(git rev-parse --short HEAD)"

echo ""
echo "=== Container pushed successfully ==="
echo ""
echo "Your CI workflows can now use: ${IMAGE}:latest"
echo ""
echo "To verify:"
echo "  docker pull ${IMAGE}:latest"
