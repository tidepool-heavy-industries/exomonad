# Task: MCP tool: get_copilot_feedback - outstanding review items

**ID:** tidepool-ddn
**Status:** open
**Priority:** 1
**Branch:** bd-ddn/mcp-tool-getcopilotfeedback-outstanding-review-items

## Description

## Vision: Consolidate PR Feedback into Single MCP Call

Agents currently make 3-4 bash invocations to piece together Copilot feedback:
```bash
gh pr view 286 --json reviews
gh pr view 286 --json comments  
gh api repos/owner/repo/pulls/286/comments
gh api repos/owner/repo/pulls/286/reviews/123/comments
```

This is slow, token-expensive, and error-prone. One MCP call should return everything.

## Target Response

```json
{
  "pr_number": 286,
  "copilot": {
    "pending": [
      {
        "id": "IC_kwDOABC123",
        "path": "src/Handler.hs",
        "line": 42,
        "body": "Consider using pattern matching here",
        "category": "suggestion",
        "in_reply_to": null
      }
    ],
    "resolved": [...]
  },
  "humans": {
    "pending": [...],
    "resolved": [...]
  },
  "summary": {
    "copilot_pending": 3,
    "copilot_resolved": 2,
    "human_pending": 0,
    "human_resolved": 1
  }
}
```

## Key Features

1. **Single call** - replaces 3-4 gh invocations
2. **Grouped by author type** - Copilot vs humans
3. **Pending vs resolved** - know what still needs addressing
4. **Inline code comments** - not just PR-level reviews
5. **Thread context** - in_reply_to for conversation threads

## Implementation

Extend existing `pr_review_status` or create new graph:
- Use GitHub effect to fetch all comment types
- Filter/group by author (copilot, github-actions[bot], humans)
- Track resolved state via GitHub's resolved conversation API
- Return structured, actionable format

## Existing Code

`pr_review_status` in PrReviewStatus.hs already fetches comments grouped by author. May be able to extend rather than replace.

## Acceptance Criteria

- [ ] Single MCP call returns all PR feedback
- [ ] Distinguishes Copilot vs human comments
- [ ] Tracks pending vs resolved state
- [ ] Includes inline code comments (not just review-level)
- [ ] Summary counts for quick triage

## Dependencies

None

## Workflow

1. Implement changes
2. Commit: [tidepool-ddn] <description>
3. Push: git push -u origin bd-ddn/mcp-tool-getcopilotfeedback-outstanding-review-items
4. File PR: Call the 'file_pr' tool (do NOT use gh cli manually)
