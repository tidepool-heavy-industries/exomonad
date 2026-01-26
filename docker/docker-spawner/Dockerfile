# Stage 1: Build
FROM rust:1.83-bookworm AS builder

WORKDIR /build

# Copy only docker-spawner crate
COPY rust/docker-spawner rust/docker-spawner
COPY rust/Cargo.lock rust/Cargo.lock

# Create a minimal workspace Cargo.toml for docker-spawner only
RUN cat > rust/Cargo.toml <<'EOF'
[workspace]
resolver = "2"
members = ["docker-spawner"]
EOF

WORKDIR /build/rust

# Build with cache mounts
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/rust/target \
    cargo build --release -p docker-spawner && \
    cp target/release/docker-spawner /docker-spawner

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /docker-spawner /usr/local/bin/docker-spawner

ENV RUST_LOG=info
EXPOSE 7435

CMD ["docker-spawner"]
