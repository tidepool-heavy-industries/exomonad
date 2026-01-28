# ExoMonad Operations Justfile
# Unified interface for Docker operations with digest pinning

# Configuration
metadata_dir := "dist"
metadata_file := metadata_dir + "/bake-metadata.json"
env_file := ".env"

# Default recipe
default:
    @just --list

# Verify jq is available
_check_jq:
    @command -v jq >/dev/null 2>&1 || { echo "ERROR: jq required but not installed"; exit 1; }

# Build and launch a service with digest pinning
dev-up target="control-server": _check_jq
    @echo ">>> [1/4] Building target '{{target}}'..."
    mkdir -p {{metadata_dir}}
    ./build {{target}} --load --metadata-file {{metadata_file}}

    @echo ">>> [2/4] Extracting digest..."
    @digest=$(jq -r '.["{{target}}"]["containerimage.digest"]' {{metadata_file}}); \
    if [ "$digest" = "null" ] || [ -z "$digest" ]; then \
        echo "ERROR: No digest for '{{target}}'"; exit 1; \
    fi; \
    echo "    Digest: $digest"

    @echo ">>> [3/4] Updating .env..."
    @env_var=$(echo "{{target}}" | tr '[:lower:]-' '[:upper:]_')_DIGEST; \
    touch {{env_file}}; \
    if grep -q "^${env_var}=" {{env_file}}; then \
        sed -i.bak "s|^${env_var}=.*|${env_var}=$digest|" {{env_file}} && rm -f {{env_file}}.bak; \
    else \
        echo "${env_var}=$digest" >> {{env_file}}; \
    fi

    @echo ">>> [4/4] Launching..."
    docker compose up -d {{target}}
    docker compose logs -f {{target}}

# Build all images (no load, just build to cache)
build:
    ./build

# Build and load all images into local daemon
build-load:
    ./build --load

# Build only dependency cachers (CI warmup)
build-deps:
    ./build deps

# Check if running containers match local git state
check-freshness service="control-server":
    ./scripts/check-freshness.sh {{service}}

# Update Rust dependencies and regenerate cargo-chef recipe
update-rust:
    @echo ">>> Updating Cargo dependencies..."
    docker run --rm -v $(pwd):/app -w /app rust:1.93-bookworm cargo update

    @echo ">>> Regenerating cargo-chef recipe..."
    docker run --rm -v $(pwd):/app -w /app/rust lukemathwalker/cargo-chef:latest \
        cargo chef prepare --recipe-path /app/recipe.json

    @echo ">>> Done. Commit 'Cargo.lock'."

# Freeze Haskell dependencies
freeze-haskell:
    @echo ">>> Freezing Haskell dependencies..."
    docker run --rm -v $(pwd):/app -w /app haskell:9.12.2-bookworm \
        sh -c "cabal update && cabal freeze"

    @echo ">>> Done. Commit 'cabal.project.freeze'."

# Run compose sync test
test-compose-sync:
    cargo test -p docker-ctl compose_sync

# Show git SHA that would be embedded
show-sha:
    @echo "Git SHA: $(git rev-parse --short HEAD)"

# Clean build artifacts
clean:
    rm -rf {{metadata_dir}}
    docker builder prune -f

# Demo recipe for JustExec effect
test-exec-demo:
    @echo "Running tests..."
    @echo '{"status": "passed", "count": 42}'