# types-first-dev

TDD workflow orchestrator using Claude Code agents.

## Quick Start

```bash
# 1. Setup Docker auth (one-time)
docker-compose up -d
docker-compose exec auth-shell claude login

# 2. Configure mantle
cp mantle-config.example.toml ~/.config/mantle/config.toml

# 3. Run
./run-actor-graph.sh specs/url-shortener.yaml
```

See [DOCKER-AUTH.md](DOCKER-AUTH.md) for detailed setup instructions.

## Running

```bash
# From types-first-dev directory
./run-actor-graph.sh                              # default spec
./run-actor-graph.sh path/to/spec.yaml            # custom spec
./run-actor-graph.sh path/to/spec.yaml ./target   # custom spec + target dir
```

## Spec Files

Specs live in `specs/` as YAML. The V2 machinery is domain-blind—all domain knowledge lives in spec files.

```yaml
# specs/url-shortener.yaml
description: |
  An effect-based URL shortener service with Servant API.
  Uses algebraic effects (freer-simple) to separate concerns.

target_path: src/UrlShortener
test_path: test/UrlShortener

acceptance_criteria:
  - |
    UrlService effect GADT with operations:
    - Shorten :: LongUrl -> UrlService ShortUrl
    - Lookup :: ShortCode -> UrlService (Maybe LongUrl)
  - Persistence effect GADT with Store, Fetch, ListAll operations
  # ...more criteria
```

**Fields:**
- `description` — What we're building (multiline markdown OK)
- `acceptance_criteria` — Must-have features (list of strings, multiline OK)
- `target_path` — Where to write code
- `test_path` — Where to write tests
- `parent_branch` — (optional) Parent git branch for children
- `depth` — (optional) Recursion depth, defaults to 0

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
