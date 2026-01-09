#!/bin/bash
# WT-3: Session Continuity Audit
# Probes what --resume actually preserves in Claude Code sessions

set -euo pipefail

# Check dependencies
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required but not installed. Please install jq first."
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ZELLIJ_CC="${SCRIPT_DIR}/../target/release/mantle"
ZELLIJ_SESSION="${ZELLIJ_SESSION:-types-first-dev}"
OUTPUT_DIR="${OUTPUT_DIR:-/tmp/session-continuity-audit}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "${GREEN}[AUDIT]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Phase 1: Setup
setup() {
    log "Phase 1: Setting up test environment..."

    mkdir -p "$OUTPUT_DIR"
    TESTDIR=$(mktemp -d "$OUTPUT_DIR/probe-XXXXX")
    echo "Initial content - created at $(date)" > "$TESTDIR/test.txt"

    log "Test directory: $TESTDIR"
    log "Test file created: $TESTDIR/test.txt"

    echo "$TESTDIR"
}

# Phase 2: First session (establish ground truth)
run_first_session() {
    local testdir="$1"
    log "Phase 2: Running first session to establish ground truth..."

    local prompt='You are participating in a session continuity audit. Please do the following steps and explain your reasoning for each:

1. Read the file test.txt using the Read tool
2. Edit test.txt to add a new line saying "Modified by Claude - audit test"
3. Run the bash command "ls -la" to list files
4. State your reasoning: "I performed these actions because the audit requested specific tool usage patterns."

After completing these steps, return {"completed": true}'

    local schema='{"type": "object", "properties": {"completed": {"type": "boolean"}}, "required": ["completed"]}'

    local result_file="$OUTPUT_DIR/session1_result.json"

    "$ZELLIJ_CC" run \
        --session "$ZELLIJ_SESSION" \
        --name "audit-session-1" \
        --model sonnet \
        --cwd "$testdir" \
        --json-schema "$schema" \
        --timeout 120 \
        --prompt "$prompt" > "$result_file" 2>&1 || true

    # Extract session ID from result
    local session_id
    session_id=$(jq -r '.session_id // empty' "$result_file" 2>/dev/null || echo "")

    if [[ -z "$session_id" ]]; then
        error "Failed to extract session_id from first session"
        cat "$result_file"
        return 1
    fi

    log "First session completed. Session ID: $session_id"
    echo "$session_id"
}

# Phase 3: Resume session (probe memory)
run_resume_session() {
    local testdir="$1"
    local session_id="$2"
    log "Phase 3: Running resume session to probe memory..."

    local prompt='WITHOUT using any tools (no Read, no Bash, no file access), answer these questions from memory about what you did in the PREVIOUS turn of this conversation:

1. What files did you edit? List the exact file paths.
2. What tools did you call? List all tool names (Read, Edit, Bash, etc).
3. What reasoning did you state for your actions? Quote it if possible.
4. How confident are you in these answers: "certain", "partial", or "none"?

Return your answers as structured JSON.'

    local schema='{
        "type": "object",
        "properties": {
            "files_edited": {"type": "array", "items": {"type": "string"}},
            "tools_called": {"type": "array", "items": {"type": "string"}},
            "reasoning_recalled": {"type": "string"},
            "confidence": {"type": "string", "enum": ["certain", "partial", "none"]}
        },
        "required": ["files_edited", "tools_called", "reasoning_recalled", "confidence"]
    }'

    local result_file="$OUTPUT_DIR/session2_result.json"

    "$ZELLIJ_CC" run \
        --session "$ZELLIJ_SESSION" \
        --name "audit-session-2" \
        --model sonnet \
        --cwd "$testdir" \
        --resume "$session_id" \
        --json-schema "$schema" \
        --timeout 120 \
        --prompt "$prompt" > "$result_file" 2>&1 || true

    log "Resume session completed. Results in: $result_file"
    echo "$result_file"
}

# Phase 4: Extract ground truth from session JSONL
extract_ground_truth() {
    local testdir="$1"
    local session_id="$2"
    log "Phase 4: Extracting ground truth from session JSONL..."

    # Derive project path: leading slash becomes dash, all other slashes become dashes
    local project_path
    project_path=$(echo "$testdir" | sed 's|^/|-|; s|/|-|g')

    local session_jsonl="$HOME/.claude/projects/${project_path}/${session_id}.jsonl"

    if [[ ! -f "$session_jsonl" ]]; then
        warn "Session JSONL not found at: $session_jsonl"
        # Try to find it
        log "Searching for session JSONL..."
        find "$HOME/.claude/projects" -name "${session_id}.jsonl" 2>/dev/null || true
        # Non-fatal: caller handles missing ground truth gracefully
        return 0
    fi

    log "Found session JSONL: $session_jsonl"

    # Extract tool calls
    local tool_calls
    tool_calls=$(jq -s '[.[] | select(.type == "assistant") | .message.content[]? | select(.type == "tool_use") | .name] | unique' "$session_jsonl" 2>/dev/null || echo "[]")

    # Extract file edits
    local file_edits
    file_edits=$(jq -s '[.[] | select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Edit") | .input.file_path] | unique' "$session_jsonl" 2>/dev/null || echo "[]")

    # Save ground truth
    local ground_truth_file="$OUTPUT_DIR/ground_truth.json"
    jq -n \
        --argjson tools "$tool_calls" \
        --argjson edits "$file_edits" \
        '{tool_calls: $tools, file_edits: $edits}' > "$ground_truth_file"

    log "Ground truth saved to: $ground_truth_file"
    cat "$ground_truth_file"

    echo "$ground_truth_file"
}

# Phase 5: Compare and report
compare_and_report() {
    local ground_truth_file="$1"
    local resume_result_file="$2"
    log "Phase 5: Comparing results..."

    echo ""
    echo "=========================================="
    echo "SESSION CONTINUITY AUDIT RESULTS"
    echo "=========================================="
    echo ""

    echo "GROUND TRUTH (from session JSONL):"
    cat "$ground_truth_file"
    echo ""

    echo "AGENT RECALL (from resume session):"
    if [[ -f "$resume_result_file" ]]; then
        jq '.structured_output // .result // .' "$resume_result_file" 2>/dev/null || cat "$resume_result_file"
    else
        echo "(no result file)"
    fi
    echo ""

    echo "=========================================="
    echo "ANALYSIS"
    echo "=========================================="

    # Compare tool calls
    local gt_tools resume_tools
    gt_tools=$(jq -r '.tool_calls | sort | join(", ")' "$ground_truth_file" 2>/dev/null || echo "unknown")
    resume_tools=$(jq -r '.structured_output.tools_called // [] | sort | join(", ")' "$resume_result_file" 2>/dev/null || echo "unknown")

    echo "Tool calls - Ground truth: $gt_tools"
    echo "Tool calls - Agent recall: $resume_tools"
    echo ""

    # Compare file edits
    local gt_edits resume_edits
    gt_edits=$(jq -r '.file_edits | sort | join(", ")' "$ground_truth_file" 2>/dev/null || echo "unknown")
    resume_edits=$(jq -r '.structured_output.files_edited // [] | sort | join(", ")' "$resume_result_file" 2>/dev/null || echo "unknown")

    echo "File edits - Ground truth: $gt_edits"
    echo "File edits - Agent recall: $resume_edits"
    echo ""

    # Agent confidence
    local confidence
    confidence=$(jq -r '.structured_output.confidence // "unknown"' "$resume_result_file" 2>/dev/null || echo "unknown")
    echo "Agent confidence: $confidence"
    echo ""

    # Reasoning recall
    local reasoning
    reasoning=$(jq -r '.structured_output.reasoning_recalled // "none"' "$resume_result_file" 2>/dev/null || echo "none")
    echo "Reasoning recalled: $reasoning"
    echo ""

    echo "=========================================="
    echo "Full results saved to: $OUTPUT_DIR"
    echo "=========================================="
}

# Main
main() {
    log "Starting Session Continuity Audit"
    log "Using zellij session: $ZELLIJ_SESSION"

    # Check mantle exists
    if [[ ! -x "$ZELLIJ_CC" ]]; then
        error "mantle not found at $ZELLIJ_CC"
        error "Build it with: cd tools/mantle && cargo build --release"
        exit 1
    fi

    local testdir session_id resume_result ground_truth

    testdir=$(setup)
    session_id=$(run_first_session "$testdir")

    if [[ -z "$session_id" ]]; then
        error "First session failed to return session ID"
        exit 1
    fi

    resume_result=$(run_resume_session "$testdir" "$session_id")
    ground_truth=$(extract_ground_truth "$testdir" "$session_id")

    if [[ -n "$ground_truth" && -f "$ground_truth" ]]; then
        compare_and_report "$ground_truth" "$resume_result"
    else
        warn "Could not extract ground truth, showing raw results only"
        echo "Resume session result:"
        cat "$resume_result"
    fi
}

main
