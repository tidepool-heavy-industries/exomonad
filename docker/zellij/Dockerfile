# Minimal Zellij container for visual multiplexing
# Human attaches here; panes run docker attach to agent containers
#
# Uses pre-built binaries from builder context:
#   - rust-builder: tui-popup
#
# Build with docker buildx bake (see docker/docker-bake.hcl)

FROM common-base

# Copy pre-built tui-popup binary from builder context
COPY --from=rust-builder /app/rust/target/release/tui-popup /usr/local/bin/tui-popup
COPY --from=rust-builder /app/rust/target/release/agent-status /usr/local/bin/agent-status
RUN chmod +x /usr/local/bin/tui-popup /usr/local/bin/agent-status

# Create config directories
RUN mkdir -p /etc/zellij/layouts

# Copy layout and config
COPY docker/zellij/layout.kdl /etc/zellij/layouts/main.kdl
COPY docker/zellij/config.kdl /etc/zellij/config.kdl
COPY docker/zellij/entrypoint.sh /usr/local/bin/service-entrypoint.sh
RUN chmod +x /usr/local/bin/service-entrypoint.sh

ENV ZELLIJ_CONFIG_DIR=/etc/zellij
ENV TERM=xterm-256color

ENTRYPOINT ["/usr/local/bin/service-entrypoint.sh"]
CMD ["zellij"]

# OCI label for version tracking (at end to not bust cache on GIT_SHA changes)
ARG GIT_SHA=unknown
LABEL org.opencontainers.image.revision=${GIT_SHA}
