#!/usr/bin/env bash
# Start an augmented Claude Code session with training data capture

set -e

cd "$(dirname "$0")"

# Check for .env
if [ ! -f .env ]; then
    echo "ERROR: .env file not found"
    echo "Copy .env.template to .env and fill in your ANTHROPIC_API_KEY"
    exit 1
fi

# Source .env for validation
source .env

if [ -z "$ANTHROPIC_API_KEY" ] || [ "$ANTHROPIC_API_KEY" = "sk-ant-..." ]; then
    echo "ERROR: ANTHROPIC_API_KEY not set in .env"
    exit 1
fi

# Create output directory if needed
mkdir -p "${TEACHING_OUTPUT_DIR:-./training-data}"

echo "Starting augmented Claude Code session..."
echo "  Teaching enabled: ${TEACHING_ENABLED:-false}"
echo "  Output dir: ${TEACHING_OUTPUT_DIR:-./training-data}"
echo ""

# Launch zellij with the production layout
zellij --layout .zellij/tidepool.kdl
