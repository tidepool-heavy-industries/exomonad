#!/bin/bash
# Test script for zellij-cc LLM node style workflows
# Tests: multi-tool-use, structured output, task-driven execution

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Ensure jq is available
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required but not installed"
    exit 1
fi

# Find zellij-cc binary (prefer release, fall back to debug)
ZELLIJ_CC="${SCRIPT_DIR}/target/release/zellij-cc"
DEBUG_ZELLIJ_CC="${SCRIPT_DIR}/target/debug/zellij-cc"

if [ ! -x "$ZELLIJ_CC" ]; then
    if [ -x "$DEBUG_ZELLIJ_CC" ]; then
        echo "Warning: Using debug build"
        ZELLIJ_CC="$DEBUG_ZELLIJ_CC"
    else
        echo "Error: zellij-cc binary not found"
        echo "Run: cargo build --release"
        exit 1
    fi
fi

# Get current zellij session
SESSION=$(zellij list-sessions 2>/dev/null | grep "(current)" | awk '{print $1}')
if [ -z "$SESSION" ]; then
    echo "Error: Not running in a zellij session"
    exit 1
fi

echo "Using zellij session: $SESSION"

# JSON schema for structured output
SCHEMA='{
  "type": "object",
  "properties": {
    "summary": { "type": "string", "description": "Brief description of directory contents" },
    "file_count": { "type": "integer", "description": "Number of files found" },
    "key_files": {
      "type": "array",
      "items": { "type": "string" },
      "description": "List of important files"
    }
  },
  "required": ["summary", "file_count", "key_files"]
}'

# Test directory (defaults to script dir; override with TEST_DIR env var)
: "${TEST_DIR:="$SCRIPT_DIR"}"
OUTPUT_FILE="/tmp/zellij-cc-workflow-test.json"

echo "Running workflow test..."
echo "  Tools: Glob,Read"
echo "  Model: haiku"
echo "  CWD: $TEST_DIR"
echo "  Output: $OUTPUT_FILE"
echo ""

# Run zellij-cc with structured output
"$ZELLIJ_CC" run \
  --session "$SESSION" \
  --name "workflow-test" \
  --model haiku \
  --tools "Glob,Read" \
  --json-schema "$SCHEMA" \
  --prompt "List the files in this Rust project directory and provide a brief summary. Focus on the main source files. Return your response as JSON matching the schema." \
  --output-file "$OUTPUT_FILE" \
  --cwd "$TEST_DIR" \
  --timeout 120

echo ""
echo "=== Raw Output ==="
jq '.' "$OUTPUT_FILE"

echo ""
echo "=== Prose Result ==="
jq -r '.result' "$OUTPUT_FILE" | head -5
echo "..."

echo ""
echo "=== Structured Output (from --json-schema) ==="
jq '.structured_output' "$OUTPUT_FILE"

echo ""
echo "=== Stderr (if any) ==="
cat "${OUTPUT_FILE%.json}.stderr" 2>/dev/null || echo "(empty)"

echo ""
echo "=== Validation ==="
# Check if structured_output matches schema
set +e
STRUCTURED=$(jq '.structured_output' "$OUTPUT_FILE")
if echo "$STRUCTURED" | jq -e '.summary and .file_count and .key_files' > /dev/null 2>&1; then
    echo "SUCCESS: structured_output matches schema"
    echo "  Summary: $(echo "$STRUCTURED" | jq -r '.summary' | head -c 100)..."
    echo "  File count: $(echo "$STRUCTURED" | jq -r '.file_count')"
    echo "  Key files: $(echo "$STRUCTURED" | jq -r '.key_files | join(", ")')"
else
    echo "WARNING: structured_output may not match expected schema"
    echo "Got: $STRUCTURED"
fi
set -e
