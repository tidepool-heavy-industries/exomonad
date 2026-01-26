# Minimal Zellij container for visual multiplexing
# Human attaches here; panes run docker attach to agent containers

# =============================================================================
# Stage 1: Rust Builder (for tui-popup)
# =============================================================================
FROM rust:bookworm AS rust-builder

WORKDIR /build

# Copy Rust workspace (only needed crates)
COPY rust/Cargo.lock ./
COPY rust/tui-popup ./tui-popup
COPY rust/tui-sidebar ./tui-sidebar
COPY rust/mantle-shared ./mantle-shared
COPY rust/hangar-config ./hangar-config

# Create minimal workspace Cargo.toml with required dependencies
RUN cat > Cargo.toml <<'EOF'
[workspace]
resolver = "2"
members = ["tui-popup", "tui-sidebar", "mantle-shared", "hangar-config"]

[workspace.dependencies]
anyhow = "1"
chrono = { version = "0.4", features = ["serde"] }
clap = { version = "4", features = ["derive", "env"] }
nix = { version = "0.29", features = ["poll", "fs", "signal"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
strum = { version = "0.26", features = ["derive"] }
tempfile = "3"
thiserror = "2"
tokio = { version = "1", features = ["full"] }
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter"] }
uuid = { version = "1", features = ["v4"] }
EOF

# Build tui-popup
RUN cargo build --release -p tui-popup && \
    cp target/release/tui-popup /usr/local/bin/tui-popup

# =============================================================================
# Stage 2: Runtime
# =============================================================================
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    gosu \
    procps \
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI only (no daemon)
RUN curl -fsSL https://get.docker.com | sh && \
    rm -rf /var/lib/apt/lists/*

# Install cargo-binstall for binary installation
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash

# Install Zellij via cargo-binstall
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo-binstall --no-confirm zellij && \
    cp /root/.cargo/bin/zellij /usr/local/bin/zellij && \
    chmod 755 /usr/local/bin/zellij

# Copy tui-popup binary from rust-builder
COPY --from=rust-builder /usr/local/bin/tui-popup /usr/local/bin/tui-popup

# Create non-root user
RUN useradd -m -u 1000 -s /bin/bash user

# Create config directories
RUN mkdir -p /etc/zellij/layouts

# Copy layout and config
COPY docker/zellij/layout.kdl /etc/zellij/layouts/main.kdl
COPY docker/zellij/config.kdl /etc/zellij/config.kdl
COPY docker/zellij/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENV ZELLIJ_CONFIG_DIR=/etc/zellij
ENV TERM=xterm-256color
ENV XDG_RUNTIME_DIR=/run/user/1000

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["zellij"]
