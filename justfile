# ExoMonad Justfile
# Run `just` to see available recipes

set shell := ["bash", "-uc"]

# Default: show available recipes
default:
    @just --list

# FIXME: TypeScript/WASM not currently used - restore when needed
# Verify protocol conformance between Haskell and TypeScript
# Requires: node/npm installed in deploy/
# test-protocol-conformance:
#     @echo "── Generating JSON samples from Haskell ──"
#     cabal run generate-golden-samples
#     @echo ""
#     @echo "── Verifying TypeScript can parse samples ──"
#     cd deploy && npx tsx test/verify-protocol.ts

# ─────────────────────────────────────────────────────────────
# Linting
# ─────────────────────────────────────────────────────────────

# Run hlint on Haskell sources (errors only, not suggestions)
lint:
    @echo "── Running hlint ──"
    hlint haskell/dsl/core haskell/runtime/wasm --ignore-suggestions

# ─────────────────────────────────────────────────────────────
# Pre-commit checks
# ─────────────────────────────────────────────────────────────

# Fast pre-commit check (build Haskell + Rust)
pre-commit-fast:
    cabal build all
    cd rust && cargo build

# Full pre-commit check (build + test Haskell + Rust)
pre-commit:
    cabal build all
    cabal test all
    cd rust && cargo test

# Build effector binary
build-effector:
    cd rust && cargo build -p effector

# Test effector binary
test-effector:
    cd rust && cargo test -p effector

# ─────────────────────────────────────────────────────────────
# WASM Roundtrip Testing
# ─────────────────────────────────────────────────────────────

# FIXME: TypeScript/WASM not currently used - restore when needed
# Build roundtrip WASM (requires: nix develop .#wasm)
# build-roundtrip-wasm:
#     @echo "── Building roundtrip WASM ──"
#     @echo "Note: This must be run inside 'nix develop .#wasm'"
#     wasm32-wasi-cabal build exomonad-reactor
#     @echo ""
#     @echo "── Copying WASM to deploy/ ──"
#     find dist-newstyle -name '*.wasm' -exec cp {} deploy/exomonad-roundtrip.wasm \;
#     @echo "✓ Built: deploy/exomonad-roundtrip.wasm"

# FIXME: TypeScript/WASM not currently used - restore when needed
# Run cross-boundary property tests (requires WASM built first)
# test-roundtrip:
#     @echo "── Running cross-boundary property tests ──"
#     cd deploy && pnpm test -- --grep "roundtrip"

# ─────────────────────────────────────────────────────────────
# Hangar Runtime
# ─────────────────────────────────────────────────────────────

# Discover Hangar root from current directory
_hangar-root:
    #!/usr/bin/env bash
    HANGAR_ROOT=$(pwd)
    while [ ! -f "$HANGAR_ROOT/Hangar.toml" ] && [ "$HANGAR_ROOT" != "/" ] && [ "$HANGAR_ROOT" != "$HOME" ]; do
        HANGAR_ROOT=$(dirname "$HANGAR_ROOT")
    done
    if [ ! -f "$HANGAR_ROOT/Hangar.toml" ]; then
        echo "ERROR: Hangar.toml not found" >&2
        exit 1
    fi
    echo "$HANGAR_ROOT"

# Rebuild runtime binaries from current repo
rebuild-runtime:
    #!/usr/bin/env bash
    set -euo pipefail

    # Find Hangar root for binary installation
    HANGAR_ROOT=$(just _hangar-root)
    RUNTIME_BIN="$HANGAR_ROOT/runtime/bin"

    # Build from current repo directory
    REPO_ROOT=$(pwd)

    echo "── Hangar: $HANGAR_ROOT ──"
    echo "── Building from current repo: $REPO_ROOT ──"

    echo ""
    echo "── Building Haskell (exomonad-control-server) ──"
    cabal build exomonad-control-server

    echo ""
    echo "── Building Rust (exomonad, tui-sidebar, tui-popup, ssh-proxy, effector) ──"
    cd "$REPO_ROOT/rust" && cargo build --release -p exomonad -p tui-sidebar -p tui-popup -p ssh-proxy -p effector

    echo ""
    echo "── Installing to $RUNTIME_BIN ──"
    cd "$REPO_ROOT"
    cp "$(cabal list-bin exomonad-control-server)" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/exomonad" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/tui-sidebar" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/tui-popup" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/ssh-proxy" "$RUNTIME_BIN/"
    cp "$REPO_ROOT/rust/target/release/effector" "$RUNTIME_BIN/"

    echo ""
    echo "── Code signing binaries (macOS) ──"
    codesign -s - --force "$RUNTIME_BIN/exomonad-control-server" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/exomonad" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/tui-sidebar" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/tui-popup" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/ssh-proxy" 2>/dev/null || true
    codesign -s - --force "$RUNTIME_BIN/effector" 2>/dev/null || true

    echo ""
    echo "✓ Runtime binaries rebuilt from current repo:"
    ls -lh "$RUNTIME_BIN"

# ─────────────────────────────────────────────────────────────
# WASM & Deployment
# ─────────────────────────────────────────────────────────────

# FIXME: TypeScript/WASM not currently used - restore when needed
# Build WASM blob (requires nix develop .#wasm)
# build-wasm:
#     #!/usr/bin/env bash
#     set -euo pipefail
#     echo "── Building exomonad-reactor with wasm32-wasi-ghc ──"
#     nix develop .#wasm --command bash -c ' \
#         wasm32-wasi-cabal build exomonad-reactor && \
#         echo "" && \
#         echo "── Optimizing WASM with wasm-opt -Oz ──" && \
#         WASM=$(find dist-newstyle/build/wasm32-wasi -name "exomonad-reactor.wasm" | head -1) && \
#         ls -lh "$WASM" && \
#         wasm-opt -Oz "$WASM" -o deploy/src/exomonad.wasm && \
#         echo "  ↓ optimized to:" && \
#         ls -lh deploy/src/exomonad.wasm \
#     '
#     echo "✓ WASM blob ready"

# FIXME: TypeScript/WASM not currently used - restore when needed
# Deploy to Cloudflare Workers
# deploy-worker:
#     @echo "── Deploying to Cloudflare Workers ──"
#     cd deploy && pnpm run deploy
#     @echo "✓ Deployed"

# FIXME: TypeScript/WASM not currently used - restore when needed
# Build WASM and deploy to Cloudflare (full pipeline)
# deploy: build-wasm deploy-worker
#     @echo ""
#     @echo "✓ Full deploy complete"

# ─────────────────────────────────────────────────────────────
# Analysis
# ─────────────────────────────────────────────────────────────

# Analyze blast radius of current branch vs base (default: main)
blast-radius base="main":
    @./tools/blast-radius.sh {{base}}

# ─────────────────────────────────────────────────────────────
# Observability
# ─────────────────────────────────────────────────────────────

# FIXME: TypeScript/WASM not currently used - restore when needed
# Tail Cloudflare Worker logs (pretty format)
# logs:
#     cd deploy && npx wrangler tail --format pretty

# FIXME: TypeScript/WASM not currently used - restore when needed
# Tail logs with JSON format (for parsing)
# logs-json:
#     cd deploy && npx wrangler tail --format json

# ─────────────────────────────────────────────────────────────
# Docker Workflows
# ─────────────────────────────────────────────────────────────

# Run pre-build steps (key generation, etc.)
docker-prebuild:
    ./scripts/docker-prebuild.sh

# Build orchestrator image
docker-build: docker-prebuild
    docker compose build

# Build agent image
docker-build-agent: docker-prebuild
    docker build -t exomonad/claude-agent:latest -f docker/claude-agent/Dockerfile .

# Build all docker images
docker-build-all: docker-build docker-build-agent

# Start orchestrator in detached mode
docker-up:
    docker compose up -d

# Start orchestrator-dev in detached mode
docker-up-dev:
    docker compose --profile dev up -d

# Attach to orchestrator Zellij session
# Auto-detects SSH transport and wraps with ssh -t for TTY passthrough
docker-attach:
    #!/usr/bin/env bash
    if [[ "$DOCKER_HOST" == ssh://* ]]; then
        # Docker's SSH transport doesn't support TTY passthrough (dial-stdio limitation)
        # Wrap with ssh -t to allocate PTY on remote host
        ssh -t "${DOCKER_HOST#ssh://}" "docker exec -it exomonad-orchestrator gosu user zellij attach orchestrator"
    else
        docker exec -it exomonad-orchestrator gosu user zellij attach orchestrator
    fi

# Spawn agent container with shared sockets and auth isolation
docker-agent BRANCH:
    #!/usr/bin/env bash
    docker run -it --rm \
      -v exomonad-sockets:/home/agent/.exomonad/sockets \
      -v ~/.claude/.credentials.json:/mnt/secrets/.credentials.json:rw \
      -e CLAUDE_CONFIG_DIR=/home/agent/.claude-agent-{{BRANCH}} \
      exomonad/claude-agent:latest

# Clean shutdown
docker-down:
    docker compose down

# Test SSH connectivity between orchestrator and agent
test-ssh: docker-prebuild
    docker compose up -d orchestrator agent-1
    @echo "Waiting for sshd to start..."
    @sleep 2
    docker compose exec orchestrator ssh -i /etc/ssh-proxy/orchestrator_key -o StrictHostKeyChecking=no root@agent-1 'echo "SSH Connection Successful: $$(hostname)"'
    docker compose stop agent-1

# One-command workflow (build + up + attach)
docker-run: docker-build docker-up docker-attach

# One-command dev workflow (build + up-dev + attach to orchestrator-dev)
docker-run-dev: docker-build docker-up-dev
    docker attach --detach-keys="ctrl-p,ctrl-q" orchestrator-dev

# Rebuild images (no cache) and restart
docker-rebuild:
    docker compose build --no-cache
    docker compose down
    docker compose up -d

# Exec into orchestrator container
docker-exec-orchestrator:
    docker exec -it exomonad-orchestrator gosu user zsh

# Exec into control-server container
docker-exec-control:
    docker exec -it exomonad-control-server zsh

# Test spawn_agents cross-container Zellij (requires running orchestrator + control-server)
docker-test-zellij:
    #!/usr/bin/env bash
    set -e
    echo "── Testing cross-container Zellij access ──"
    echo ""
    echo "1. Checking ZELLIJ_SESSION_NAME in control-server..."
    docker exec exomonad-control-server bash -c 'echo "ZELLIJ_SESSION_NAME=$ZELLIJ_SESSION_NAME"'
    echo ""
    echo "2. Checking XDG_RUNTIME_DIR shared volume..."
    docker exec exomonad-control-server ls -la /run/user/1000/zellij/ 2>/dev/null || echo "   (Zellij session may not be running yet)"
    echo ""
    echo "3. Testing zellij CLI in control-server..."
    docker exec exomonad-control-server which zellij
    docker exec exomonad-control-server zellij --version
    echo ""
    echo "4. Listing Zellij sessions from control-server..."
    docker exec exomonad-control-server zellij list-sessions 2>/dev/null || echo "   (No sessions found or Zellij not running)"
    echo ""
    echo "✓ Cross-container Zellij setup verified"

# Show logs from control-server container
docker-logs-control:
    docker logs -f exomonad-control-server

# Show logs from orchestrator container
docker-logs-orchestrator:
    docker logs -f exomonad-orchestrator

