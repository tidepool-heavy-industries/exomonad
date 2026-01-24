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

# Generate self-signed SSL certificate for web server
# Zellij requires SSL when binding to non-localhost addresses
# Store in /etc/ssl to match config.kdl paths
mkdir -p /etc/ssl/zellij
openssl req -x509 -newkey rsa:4096 -nodes \
    -keyout /etc/ssl/zellij/key.pem \
    -out /etc/ssl/zellij/cert.pem \
    -days 365 -subj "/CN=tidepool-orchestrator" 2>/dev/null

echo "🌐 Zellij will start web server on 0.0.0.0:8080 (configured in config.kdl)"
echo "📋 After startup, get login token with: docker exec tidepool-orchestrator zellij web --create-token"
echo "🌍 Then open: https://nixos:8080/orchestrator"

# Launch Zellij session
# Web server will be started automatically by Zellij based on config.kdl settings
# attach --create will create the session if it doesn't exist, or attach if it does
exec zellij --config /etc/tidepool/zellij/config.kdl --layout /etc/tidepool/zellij/layouts/default.kdl attach --create orchestrator
