#!/bin/bash
set -euo pipefail

# Cleanup sibling containers on exit
# We use the label tidepool.orchestrator=$HOSTNAME to identify containers 
# started by this specific orchestrator instance.
cleanup() {
    echo "Cleaning up sibling containers..."
    # Using docker ps with filter to find siblings
    # xargs -r avoids errors if no containers match
    docker ps -q --filter "label=tidepool.orchestrator=$HOSTNAME" | xargs -r docker kill
}

trap cleanup EXIT

# Ensure log file exists so tail doesn't complain
touch /var/log/tidepool/control-server.log

# Ensure Zellij config directory exists
mkdir -p ~/.config/zellij

# Launch Zellij
# --create ensures a new session is started if one doesn't exist
# It will use the default layout at /root/.config/zellij/layouts/default.kdl
exec zellij attach --create orchestrator
