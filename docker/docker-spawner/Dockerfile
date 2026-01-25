# Stage 1: Build
FROM rust:1.83-bookworm AS builder

WORKDIR /build
COPY rust/docker-spawner rust/docker-spawner
COPY rust/Cargo.toml rust/Cargo.toml
COPY rust/Cargo.lock rust/Cargo.lock

WORKDIR /build/rust
RUN cargo build --release -p docker-spawner

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    docker.io \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/rust/target/release/docker-spawner /usr/local/bin/

ENV RUST_LOG=info
EXPOSE 7435

CMD ["docker-spawner"]