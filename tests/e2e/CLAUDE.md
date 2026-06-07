# E2E Tests

Documentation and conventions for running and adding end-to-end tests.

```bash
# E2E tests (interactive — launches tmux session, you observe)
just e2e-messaging         # Teams inbox delivery pipeline
just e2e-hook-rewrite      # BeforeModel/AfterModel PII rewriting
```

### E2E Test Pattern

All E2E tests live in `tests/e2e/{name}/` and follow the same structure:

**Files:**
| File | Purpose |
|------|---------|
| `run.sh` | Setup script: creates temp repo, configures companions, runs `exomonad init` |
| `testrunner.md` | Test plan for the Claude testrunner companion (copied to `.exo/roles/devswarm/context/testrunner.md`) |
| `e2e-test.md` | Root TL rules for this test (copied to `.claude/rules/e2e-test.md`) |

**Structure of `run.sh`:**
1. **Preconditions** — Check `exomonad` binary, WASM plugins, `tmux`, `git`
2. **Temp environment** — `mktemp -d`, bare remote, working repo, `exomonad new`, symlink WASM
3. **Config** — Write `config.toml` with `yolo = true`, companions for the test scenario
4. **`exomonad init`** — Last line of the script. Creates tmux session, starts server, spawns companions, attaches.

**Companion roles:**
- **Test subject** — The agent being tested (e.g., Gemini with dev role for hook rewriting)
- **Testrunner** — Claude (haiku) companion with `testrunner` role. Observes results via bash (read-only), reports via `notify_parent`

**Key conventions:**
- `shell_command = "bash"` (not nix develop — temp env has no flake)
- `yolo = true` (skip interactive prompts)
- `export GITHUB_TOKEN="test-token-e2e"` (dummy token to avoid auth errors)
- Cleanup via `trap cleanup EXIT` (kills tmux session, removes temp dir)
- Testrunner uses only `notify_parent` MCP tool + read-only bash observation
- Root TL creates a team and idles

**Adding a new E2E test:**
1. Create `tests/e2e/{name}/run.sh` following the pattern above
2. Create `testrunner.md` with the test plan (phases, assertions, report format)
3. Create `e2e-test.md` with root TL rules (usually: create team, idle)
4. Add `just e2e-{name}` recipe to `justfile`
