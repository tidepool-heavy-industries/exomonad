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

# Ensure Zellij config directory exists (if using home-based config)
# mkdir -p ~/.config/zellij

# Start Zellij web server in daemon mode
# Web server runs on port 8080, bound to 0.0.0.0 for Docker access
# Users can connect via browser at http://nixos:8080
zellij web --daemonize --ip 0.0.0.0 --port 8080

# Create a web login token (printed to logs for first-time access)
echo "🌐 Creating web login token..."
zellij web --create-token orchestrator-web || true

# Launch Zellij TUI session
# attach --create will create the session if it doesn't exist, or attach if it does
exec zellij --config /etc/tidepool/zellij/config.kdl --layout /etc/tidepool/zellij/layouts/default.kdl attach --create orchestrator
