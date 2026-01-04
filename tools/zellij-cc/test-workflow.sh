#!/bin/bash
# Test script for zellij-cc LLM node style workflows
# Tests: multi-tool-use, structured output, task-driven execution

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ZELLIJ_CC="${SCRIPT_DIR}/target/release/zellij-cc"

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

# Test directory
TEST_DIR="/home/inanna/dev/tidepool/worktrees/micro-gastown/tools/zellij-cc"
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
cat "$OUTPUT_FILE" | jq '.'

echo ""
echo "=== Prose Result ==="
cat "$OUTPUT_FILE" | jq -r '.result' | head -5
echo "..."

echo ""
echo "=== Structured Output (from --json-schema) ==="
cat "$OUTPUT_FILE" | jq '.structured_output'

echo ""
echo "=== Stderr (if any) ==="
cat "${OUTPUT_FILE%.json}.stderr" 2>/dev/null || echo "(empty)"

echo ""
echo "=== Validation ==="
# Check if structured_output matches schema
STRUCTURED=$(cat "$OUTPUT_FILE" | jq '.structured_output')
if echo "$STRUCTURED" | jq -e '.summary and .file_count and .key_files' > /dev/null 2>&1; then
    echo "SUCCESS: structured_output matches schema"
    echo "  Summary: $(echo "$STRUCTURED" | jq -r '.summary' | head -c 100)..."
    echo "  File count: $(echo "$STRUCTURED" | jq -r '.file_count')"
    echo "  Key files: $(echo "$STRUCTURED" | jq -r '.key_files | join(", ")')"
else
    echo "WARNING: structured_output may not match expected schema"
    echo "Got: $STRUCTURED"
fi
