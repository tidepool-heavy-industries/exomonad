# ==========================================================================
# Stage 1: Build Rust binaries (exomonad)
# ==========================================================================
FROM rust:1.88-bookworm AS rust-builder

RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    libcurl4-openssl-dev \
    protobuf-compiler \
    cmake \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY Cargo.toml Cargo.lock ./
COPY rust/ rust/
COPY vendor/ vendor/
COPY proto/ proto/

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/target \
    cargo build --release -p exomonad \
    && cp target/release/exomonad /tmp/exomonad

# ==========================================================================
# Stage 2: Build Haskell WASM guest (via nix + GHC WASM cross-compiler)
# ==========================================================================
FROM nixos/nix:latest AS wasm-builder

# Enable flakes
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

WORKDIR /build

# Copy flake first for better caching (flake inputs change rarely)
COPY flake.nix flake.lock ./

# Prefetch flake inputs (cached unless flake.lock changes)
RUN --mount=type=cache,target=/nix-cache \
    cp -r /nix-cache/store/* /nix/store/ 2>/dev/null || true \
    && nix flake prefetch . \
    && cp -r /nix/store/* /nix-cache/store/ 2>/dev/null || true

# Copy Haskell sources
COPY haskell/ haskell/
COPY .exo/roles/ .exo/roles/
COPY .exo/lib/ .exo/lib/
COPY cabal.project.wasm ./

# Build WASM guest
# Note: nix store cache copy uses || true because the cache dir may be empty.
# The actual build commands must NOT be swallowed by || true.
RUN --mount=type=cache,target=/nix-cache \
    --mount=type=cache,target=/root/.cabal \
    cp -r /nix-cache/store/* /nix/store/ 2>/dev/null || true \
    && nix develop .#wasm --command bash -c '\
        update_ok=false; \
        for i in 1 2 3; do \
            wasm32-wasi-cabal update --project-file=cabal.project.wasm hackage.haskell.org && update_ok=true && break; \
            echo "cabal update attempt $i failed, retrying..."; sleep 5; \
        done; \
        $update_ok || { echo "FATAL: cabal update failed after 3 attempts"; exit 1; }; \
        wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest-devswarm' \
    && cp $(find dist-newstyle -name "wasm-guest-devswarm.wasm" -type f -print -quit) /tmp/wasm-guest-devswarm.wasm \
    ; nix_cache_rc=$?; cp -r /nix/store/* /nix-cache/store/ 2>/dev/null || true; exit $nix_cache_rc

# ==========================================================================
# Stage 3: Runtime
# ==========================================================================
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Core system dependencies
RUN apt-get update && apt-get install -y \
    git \
    curl \
    ca-certificates \
    bash \
    libssl3t64 \
    libssl-dev \
    libcurl4t64 \
    pkg-config \
    build-essential \
    jq \
    tmux \
    && rm -rf /var/lib/apt/lists/*

# gh CLI (GitHub's official apt repository)
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
      | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
      > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update && apt-get install -y gh \
    && rm -rf /var/lib/apt/lists/*

# Node.js 22 LTS (needed for Gemini CLI)
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Gemini CLI (npm — no native installer yet)
RUN npm install -g @google/gemini-cli

# --------------------------------------------------------------------------
# Non-root user (Claude Code refuses to run as root)
# Ubuntu 24.04 ships a 'ubuntu' user at UID 1000. We need UID 1000 so
# bind-mounted host files (~/.claude, ~/.gemini) are readable.
# --------------------------------------------------------------------------
RUN userdel -r ubuntu 2>/dev/null; useradd -m -s /bin/bash -u 1000 exo
RUN mkdir -p /tmp/runtime \
    && chown -R exo:exo /home/exo /tmp/runtime

# --------------------------------------------------------------------------
# Environment
# --------------------------------------------------------------------------
RUN mkdir -p /workspace && chown exo:exo /workspace

USER exo
ENV SHELL=/bin/bash
ENV HOME=/home/exo
ENV XDG_RUNTIME_DIR=/tmp/runtime
ENV TERM=xterm-256color

# Rust toolchain via rustup (installed as exo user so cargo is in ~/.cargo/bin)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --default-toolchain stable --profile minimal
ENV PATH="/home/exo/.cargo/bin:${PATH}"

# Claude Code (native installer — npm package is deprecated)
# Installs to ~/.local/bin/claude (outside ~/.claude/), so the full
# ~/.claude/ bind mount from run.sh doesn't clobber the binary.
RUN curl -fsSL https://claude.ai/install.sh | bash
ENV PATH="/home/exo/.local/bin:${PATH}"

# --------------------------------------------------------------------------
# Binaries + artifacts from builder stages
# --------------------------------------------------------------------------
USER root
COPY --from=rust-builder /tmp/exomonad /usr/local/bin/exomonad
COPY --from=wasm-builder /tmp/wasm-guest-devswarm.wasm /opt/exomonad/wasm/
COPY try-exomonad/bootstrap.sh /usr/local/bin/exomonad-bootstrap
RUN chmod +x /usr/local/bin/exomonad-bootstrap
USER exo

WORKDIR /workspace
CMD ["bash"]
