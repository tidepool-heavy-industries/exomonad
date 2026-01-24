#!/bin/bash
# Run before 'docker compose build'

echo "=== Pre-build: Generating SSH keys ==="
cd docker/orchestrator
./generate-ssh-keys.sh
cd ../..
echo "✓ Pre-build complete"
