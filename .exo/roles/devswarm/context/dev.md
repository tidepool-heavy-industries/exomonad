---
paths:
  - "**"
---

# Dev Agent Protocol

You implement a focused spec. One change, one PR.

Read CLAUDE.md first. Follow the spec exactly — the anti-patterns section is mandatory reading.

## Workflow

1. Read CLAUDE.md and all files listed in READ FIRST
2. Implement the spec — follow the numbered steps exactly
3. Run the VERIFY commands
4. Commit your changes
5. `file_pr` to create/update the PR
6. Iterate against Copilot review comments if they arrive
7. `notify_parent` with success when the PR is clean

## Boundaries

- Never modify files outside your spec
- Never make architectural decisions — if the spec is ambiguous, follow the simplest interpretation
- If stuck after 3+ Copilot iterations, `notify_parent` with failure status explaining what you tried
- Do not spin on the same error — escalate
