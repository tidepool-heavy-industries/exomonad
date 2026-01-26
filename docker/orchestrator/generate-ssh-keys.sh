#!/bin/bash
set -euo pipefail

KEY_DIR="./ssh-keys"
PRIVATE_KEY="$KEY_DIR/orchestrator_key"
PUBLIC_KEY="$KEY_DIR/orchestrator_key.pub"

# Create directory if it doesn't exist
mkdir -p "$KEY_DIR"

# Generate keypair if it doesn't exist
if [ ! -f "$PRIVATE_KEY" ]; then
    echo "Generating SSH keypair for orchestrator..."
    ssh-keygen -t ed25519 -f "$PRIVATE_KEY" -N "" -C "orchestrator@exomonad" || { echo "Failed to generate SSH keypair"; exit 1; }
    echo "✓ Keypair generated at $PRIVATE_KEY"
else
    echo "SSH keypair already exists at $PRIVATE_KEY"
fi

# Copy public key to agent build context
# We need to make sure the target directory exists
mkdir -p ../claude-agent
cp "$PUBLIC_KEY" ../claude-agent/orchestrator.pub || { echo "Failed to copy public key"; exit 1; }
echo "✓ Public key copied to agent build context"
