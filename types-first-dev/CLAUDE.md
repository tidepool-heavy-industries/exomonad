# types-first-dev

TDD workflow orchestrator using Claude Code agents.

## Rubric Design Principle

**LLM is sensor, code is controller.**

The LLM reports structured fields describing what it did. The handler code decides if that's good enough. This separation:
- Saves tokens (routing logic never in prompt)
- Keeps policy in version-controlled code
- Enables mechanical evaluation

### Two-Stream Architecture

1. **Mechanical checks** - Handler computes these (build status, grep for undefined, test results). Never asks LLM.
2. **Semantic rubrics** - LLM reports these (approach taken, open questions, unhandled cases). Things we can't mechanically derive.

### Value-Neutral Field Design

**Key insight: Ask for useful information without hinting at consequences.**

The LLM has no reason to game responses when it doesn't know what we'll do with the answers.

| Don't ask | Do ask |
|-----------|--------|
| `confidence: 1-5` (inflatable) | `openQuestions: [Text]` (list length = derived metric) |
| `edgeCaseCoverage: 1-5` | `unhandledCases: [Text]` (content is actionable) |
| `completeness: "complete"` | (handler greps for undefined) |
| `severity: "critical"` | `attemptedSolutions: [Text]` + `wouldUnblock: Text` (infer severity) |

Every rubric field should be:
- **Semantic** - Can't be mechanically derived (otherwise handler should compute it)
- **Neutral** - No value ordering to optimize for
- **Actionable** - Non-empty list tells us exactly what to do next
