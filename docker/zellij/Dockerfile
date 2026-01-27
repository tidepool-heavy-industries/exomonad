# Minimal Zellij container for visual multiplexing
# Human attaches here; panes run docker attach to agent containers
#
# Uses pre-built binaries from builder context:
#   - rust-builder: tui-popup
#
# Build with docker buildx bake (see docker/docker-bake.hcl)

FROM common-base

# Copy pre-built binaries from builder context
COPY --from=rust-builder /app/rust/target/release/tui-popup /usr/local/bin/tui-popup
COPY --from=rust-builder /app/rust/target/release/agent-status /usr/local/bin/agent-status
COPY --from=rust-builder /app/rust/target/release/zellij-gen /usr/local/bin/zellij-gen
RUN chmod +x /usr/local/bin/tui-popup /usr/local/bin/agent-status /usr/local/bin/zellij-gen

# Create config directories (layouts generated at runtime by zellij-gen)
RUN mkdir -p /etc/zellij/layouts

# Copy config (layouts are generated dynamically by zellij-gen in entrypoint)
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
