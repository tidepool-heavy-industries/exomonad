# Task: Documentation sweep: CLAUDE.md + ADR for MCP patterns

**ID:** tidepool-4fu
**Status:** open
**Priority:** 2
**Branch:** bd-4fu/documentation-sweep-claudemd-adr-for-mcp-patterns

## Description

## Summary

Post-Wave-1 documentation update to capture new tools and patterns.

## Scope

1. **haskell/control-server/CLAUDE.md**
   - Add pm_status, pm_review_dag to MCP Tools section
   - Update tool count (was 7, now 9+)
   - Document role-based filtering pattern

2. **ADR: MCP Tool Design Patterns**
   - Tier 1 (LSP-only) vs Tier 2 (LLM-enhanced) vs Tier 4 (TUI-interactive)
   - GraphEntries vs MCPExport annotation patterns
   - Schema-shaped cognition (ADR 002 reference)

3. **rust/mantle-agent/CLAUDE.md**
   - Document --tools flag usage for PM vs TL roles
   - Add troubleshooting section for tool filtering

## Acceptance Criteria
- [ ] All new MCP tools documented in control-server CLAUDE.md
- [ ] ADR written and linked from relevant docs
- [ ] mantle-agent docs updated with role filtering examples

## Dependencies

None

## Workflow

1. Implement changes
2. Commit: [tidepool-4fu] <description>
3. Push: git push -u origin bd-4fu/documentation-sweep-claudemd-adr-for-mcp-patterns
4. File PR: Call the 'file_pr' tool (do NOT use gh cli manually)
