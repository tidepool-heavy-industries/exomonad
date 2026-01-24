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
mkdir -p /tmp/zellij-ssl
openssl req -x509 -newkey rsa:4096 -nodes \
    -keyout /tmp/zellij-ssl/key.pem \
    -out /tmp/zellij-ssl/cert.pem \
    -days 365 -subj "/CN=tidepool-orchestrator" 2>/dev/null

# Fix permissions so Zellij can read the certificate files
chmod 644 /tmp/zellij-ssl/cert.pem
chmod 600 /tmp/zellij-ssl/key.pem

# Start Zellij web server in background (using & instead of --daemonize to see errors)
# Web server runs on port 8080, bound to 0.0.0.0 for Docker access
# Users can connect via browser at https://nixos:8080/orchestrator (note: HTTPS with self-signed cert)
echo "🌐 Starting Zellij web server..."
zellij web --ip 0.0.0.0 --port 8080 \
    --cert /tmp/zellij-ssl/cert.pem \
    --key /tmp/zellij-ssl/key.pem &

# Wait for web server to stabilize
sleep 2

# Create a web login token (printed to logs for first-time access)
echo "🌐 Creating web login token..."
zellij web --create-token
echo "📋 Copy the token above to log into https://nixos:8080/orchestrator"

# Launch Zellij TUI session
# attach --create will create the session if it doesn't exist, or attach if it does
exec zellij --config /etc/tidepool/zellij/config.kdl --layout /etc/tidepool/zellij/layouts/default.kdl attach --create orchestrator
