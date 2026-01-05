#!/usr/bin/env bash
# Blast radius analysis for PRs
# Usage: ./tools/blast-radius.sh [base-branch]
# Default base branch: main

set -euo pipefail

BASE_BRANCH="${1:-main}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Build impact-analysis if needed
echo "Building impact-analysis..."
cabal build impact-analysis >/dev/null 2>&1
IMPACT_TOOL=$(cabal list-bin impact-analysis)

# Get changed files
echo "Comparing HEAD to $BASE_BRANCH..."
changed_files=$(git diff --name-only "$BASE_BRANCH"...HEAD -- '*.hs' 2>/dev/null || git diff --name-only "$BASE_BRANCH" HEAD -- '*.hs')

if [ -z "$changed_files" ]; then
  echo "No Haskell files changed."
  exit 0
fi

echo ""
echo "# Blast Radius Report"
echo ""
echo "Analyzing impact of changes vs \`$BASE_BRANCH\`."
echo ""

total_refs=0
declare -A all_affected_files

# Process each changed file
while IFS= read -r file; do
  [ -z "$file" ] && continue
  [ ! -f "$file" ] && continue

  echo "## \`$file\`"
  echo ""

  # Get actual line numbers from the unified diff
  line_numbers=$(git diff "$BASE_BRANCH"...HEAD -U0 -- "$file" 2>/dev/null || git diff "$BASE_BRANCH" HEAD -U0 -- "$file" | \
    awk '
      /^@@/ {
        gsub(/.*\+/, "", $0)
        gsub(/,.*/, "", $0)
        gsub(/ .*/, "", $0)
        line = int($0)
        next
      }
      /^\+[^+]/ {
        if ($0 ~ /::/ || $0 ~ /^.[a-z][a-zA-Z0-9_]*[ \t]*=/ || $0 ~ /^.data[ \t]/ || $0 ~ /^.type[ \t]/ || $0 ~ /^.newtype[ \t]/ || $0 ~ /^.class[ \t]/) {
          print line
        }
        line++
      }
      /^-/ { next }
      /^ / { line++ }
    ' | head -10)

  if [ -z "$line_numbers" ]; then
    echo "_No analyzable definitions found._"
    echo ""
    continue
  fi

  echo "| Symbol | References | Files Affected |"
  echo "|--------|------------|----------------|"

  file_had_results=false

  for line_num in $line_numbers; do
    # Run impact analysis
    result=$("$IMPACT_TOOL" --file "$file" --line "$line_num" --col 1 --format json 2>/dev/null || echo '{}')

    # Parse JSON output
    symbol_name=$(echo "$result" | jq -r '.symbol.symbolName // empty' 2>/dev/null)
    ref_count=$(echo "$result" | jq -r '.stats.totalReferences // 0' 2>/dev/null)
    files_affected=$(echo "$result" | jq -r '.stats.filesAffected // 0' 2>/dev/null)

    if [ -n "$symbol_name" ] && [ "$symbol_name" != "null" ] && [[ ! "$symbol_name" =~ ^Error: ]]; then
      file_had_results=true
      echo "| \`$symbol_name\` | $ref_count | $files_affected |"

      total_refs=$((total_refs + ref_count))

      # Track affected files
      affected=$(echo "$result" | jq -r '.stats.fileBreakdown[][0] // empty' 2>/dev/null)
      for af in $affected; do
        all_affected_files["$af"]=1
      done
    fi
  done

  if [ "$file_had_results" = false ]; then
    echo ""
    echo "_Could not analyze definitions (HLS may need more context)._"
  fi

  echo ""

done <<< "$changed_files"

# Summary
total_files=${#all_affected_files[@]}

echo "---"
echo ""
echo "### Summary"
echo ""
echo "- **Total references to changed symbols:** $total_refs"
echo "- **Unique files in blast radius:** $total_files"
echo ""

if [ $total_refs -gt 50 ]; then
  echo "> **High impact change** - Consider additional review for changes affecting 50+ references."
elif [ $total_refs -gt 20 ]; then
  echo "> **Moderate impact** - Changes affect multiple call sites."
else
  echo "> **Low impact** - Localized change."
fi
