# Multi-stage Dockerfile for Tidepool Orchestrator

# Stage 1: Extract Docker CLI
# We use the official Docker image to get a static docker binary
FROM docker:cli AS docker-cli

# Stage 2: Main Orchestrator Image
FROM ubuntu:22.04

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
# tini: for zombie reaping and signal handling (PID 1)
# curl, ca-certificates: for downloading assets and secure connections
# bash: shell for scripts
# git: for repository management within the orchestrator
RUN apt-get update && apt-get install -y \
    tini \
    curl \
    ca-certificates \
    bash \
    git \
    socat \
    lsof \
    && rm -rf /var/lib/apt/lists/*

# Install Docker CLI from the first stage
COPY --from=docker-cli /usr/local/bin/docker /usr/local/bin/docker

# Install Zellij
ARG ZELLIJ_VERSION=0.41.2
RUN curl -L https://github.com/zellij-org/zellij/releases/download/v${ZELLIJ_VERSION}/zellij-x86_64-unknown-linux-musl.tar.gz | tar xz \
    && mv zellij /usr/local/bin/zellij \
    && chmod +x /usr/local/bin/zellij

# Copy pre-built Tidepool binaries
# NOTE: These must be Linux x86_64 binaries placed in docker/orchestrator/bin/ before building.
# We copy the directory first and then move contents to avoid COPY wildcard failures if empty.
COPY bin/ /tmp/bin/
RUN if [ "$(ls -A /tmp/bin)" ]; then cp /tmp/bin/* /usr/local/bin/ && chmod +x /usr/local/bin/*; fi

# Copy Zellij configurations and layouts
COPY config/orchestrator.kdl /root/.config/zellij/layouts/default.kdl
COPY config/zellij.kdl /root/.config/zellij/config.kdl

# Copy and set up the entrypoint script
COPY scripts/orchestrator-entrypoint.sh /usr/local/bin/orchestrator-entrypoint.sh
RUN chmod +x /usr/local/bin/orchestrator-entrypoint.sh

# Environment variables for Tidepool components
ENV TIDEPOOL_CONTROL_SOCKET=/sockets/control.sock
ENV TIDEPOOL_TUI_SOCKET=/sockets/tui.sock
ENV HANGAR_ROOT=/worktrees
ENV TERM=xterm-256color
ENV DOCKER_HOST=unix:///var/run/docker.sock

# Create necessary directories with appropriate permissions
RUN mkdir -p /sockets /worktrees /var/log/tidepool && \
    chmod 777 /sockets /worktrees /var/log/tidepool

WORKDIR /worktrees

# Use tini as the entrypoint for proper signal handling
ENTRYPOINT ["/usr/bin/tini", "--"]

# Launch the orchestrator entrypoint script
CMD ["/usr/local/bin/orchestrator-entrypoint.sh"]
