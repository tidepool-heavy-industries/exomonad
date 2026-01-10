# Build mantle-agent for Linux inside Docker
# From rust/ directory: docker build -t mantle-agent:latest -f mantle/Dockerfile .

FROM rustlang/rust:nightly-bookworm AS builder

WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock ./
COPY mantle ./mantle
COPY mantle-agent ./mantle-agent
COPY mantle-shared ./mantle-shared

# Build mantle-agent
RUN cargo build --release -p mantle-agent

# Runtime stage
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/target/release/mantle-agent /usr/local/bin/

ENTRYPOINT ["mantle-agent"]
