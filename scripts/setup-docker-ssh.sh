#!/bin/bash
set -euo pipefail

# Setup script for remote Docker SSH keys
# This copies your local SSH key to the exomonad-ssh-keys volume on the Docker host
#
# Usage: ./scripts/setup-docker-ssh.sh [path-to-ssh-key]
# Default: ~/.ssh/id_ed25519

SSH_KEY="${1:-$HOME/.ssh/id_ed25519}"

if [ ! -f "$SSH_KEY" ]; then
    echo "❌ SSH key not found: $SSH_KEY"
    echo "   Usage: $0 [path-to-ssh-key]"
    exit 1
fi

echo "🔑 Setting up SSH key volume for remote Docker..."
echo "   Key: $SSH_KEY"
echo ""

# Create the volume if it doesn't exist
docker volume create exomonad-ssh-keys 2>/dev/null || true

# Run a helper container to populate the volume (need openssh for ssh-keyscan)
echo "📦 Creating helper container..."
docker run -d --name exomonad-ssh-setup \
    -v exomonad-ssh-keys:/ssh \
    alpine sh -c "apk add --no-cache openssh-client && sleep infinity"

# Wait for package install
sleep 3

# Copy the key
echo "📋 Copying SSH key to volume..."
docker cp "$SSH_KEY" exomonad-ssh-setup:/ssh/id_ed25519

# Add GitHub to known_hosts
echo "🌐 Adding GitHub to known_hosts..."
docker exec exomonad-ssh-setup sh -c "ssh-keyscan -t ed25519 github.com > /ssh/known_hosts 2>/dev/null"

# Fix permissions
echo "🔒 Setting permissions..."
docker exec exomonad-ssh-setup sh -c "chmod 700 /ssh && chmod 600 /ssh/id_ed25519 && chmod 644 /ssh/known_hosts"

# Cleanup
echo "🧹 Cleaning up..."
docker rm -f exomonad-ssh-setup

echo ""
echo "✅ SSH key volume ready!"
echo "   Run: docker compose up -d orchestrator"
