# Dedicated Control Server Container
# MCP server (TCP 7432) + Zellij tab creation + docker-ctl integration
#
# Uses pre-built binaries from builder contexts:
#   - haskell-builder: exomonad-control-server
#   - rust-builder: docker-ctl, tui-spawner
#
# Build with docker buildx bake (see docker/docker-bake.hcl)

FROM common-base

# Install additional runtime dependencies (on top of common-base)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 \
    libffi8 \
    zlib1g \
    socat \
    && rm -rf /var/lib/apt/lists/*

# Copy pre-built binaries from builder contexts
# These come from the BuildKit contexts defined in docker-bake.hcl
COPY --from=haskell-builder /out/exomonad-control-server /usr/local/bin/exomonad-control-server
COPY --from=rust-builder /app/rust/target/release/docker-ctl /usr/local/bin/docker-ctl
COPY --from=rust-builder /app/rust/target/release/tui-spawner /usr/local/bin/tui-spawner
COPY --from=rust-builder /app/rust/target/release/service-server /usr/local/bin/service-server

# Verify binaries are executable
RUN chmod +x /usr/local/bin/exomonad-control-server \
             /usr/local/bin/docker-ctl \
             /usr/local/bin/tui-spawner \
             /usr/local/bin/service-server

# Copy service-specific entrypoint wrapper
COPY docker/control-server/entrypoint.sh /usr/local/bin/service-entrypoint.sh
RUN chmod +x /usr/local/bin/service-entrypoint.sh

# Environment (extends common-base)
ENV EXOMONAD_CONTROL_SOCKET=/sockets/control.sock
ENV EXOMONAD_TUI_SOCKET=/sockets/tui.sock
ENV EXOMONAD_SERVICE_SOCKET=/sockets/services.sock
ENV ZELLIJ_SESSION_NAME=main
# Override GIT_SAFE_DIRECTORY for repo bind mount
ENV GIT_SAFE_DIRECTORY=/repo

# Create necessary directories (user already exists from common-base)
RUN mkdir -p /sockets && chown -R user:user /sockets

EXPOSE 7432

WORKDIR /home/user

# Use service entrypoint which calls common entrypoint internally
ENTRYPOINT ["/usr/local/bin/service-entrypoint.sh"]

# OCI label for version tracking (at end to not bust cache on GIT_SHA changes)
ARG GIT_SHA=unknown
LABEL org.opencontainers.image.revision=${GIT_SHA}
