# mantle-hub Dockerfile
# Session visualization daemon with SQLite persistence
# Build from tidepool root: docker build -t tidepool/mantle-hub:latest -f rust/mantle-hub/Dockerfile .

FROM rust:latest AS builder

WORKDIR /app

# Copy workspace files (all members required for workspace resolution)
COPY rust/Cargo.toml rust/Cargo.lock ./
COPY rust/mantle ./mantle
COPY rust/mantle-agent ./mantle-agent
COPY rust/mantle-hub ./mantle-hub
COPY rust/mantle-shared ./mantle-shared

# Build release binary
RUN cargo build --release -p mantle-hub

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libsqlite3-0 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash hub
USER hub
WORKDIR /home/hub

# Copy binary
COPY --from=builder /app/target/release/mantle-hub /usr/local/bin/mantle-hub

# Copy static files
COPY rust/mantle-hub/static /usr/local/bin/static

# Data directory for SQLite
RUN mkdir -p /home/hub/.tidepool

EXPOSE 7433

ENV RUST_LOG=info

CMD ["mantle-hub", "serve", "--port", "7433"]
