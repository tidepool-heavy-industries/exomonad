# Schema Design

Single source of truth for Rust ↔ Haskell boundary types.

## Location

```
schema/
├── effects.json          # Effect ADT definitions
├── mcp.json              # MCP request/response types
├── hooks.json            # Hook input/output types
└── common.json           # Shared types (referenced via $ref)
```

## Tagging Convention

All sum types use **adjacently tagged** encoding:

```json
{
  "kind": "VariantName",
  "payload": { ... }
}
```

This maps to:
- Rust: `#[serde(tag = "kind", content = "payload")]`
- Haskell: `sumEncoding = TaggedObject "kind" "payload"`

## Effect Schema

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "effects.json",
  "title": "ExoMonad Effects",

  "definitions": {
    "Effect": {
      "oneOf": [
        { "$ref": "#/definitions/GitGetBranch" },
        { "$ref": "#/definitions/GitGetWorktree" },
        { "$ref": "#/definitions/GitHubListIssues" },
        { "$ref": "#/definitions/GitHubCreatePR" },
        { "$ref": "#/definitions/Log" },
        { "$ref": "#/definitions/DockerExec" }
      ]
    },

    "GitGetBranch": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "GitGetBranch" },
        "payload": {
          "type": "object",
          "required": ["workingDir"],
          "properties": {
            "workingDir": { "type": "string" }
          }
        }
      }
    },

    "GitGetWorktree": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "GitGetWorktree" },
        "payload": {
          "type": "object",
          "required": ["workingDir"],
          "properties": {
            "workingDir": { "type": "string" }
          }
        }
      }
    },

    "GitHubListIssues": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "GitHubListIssues" },
        "payload": {
          "type": "object",
          "required": ["repo"],
          "properties": {
            "repo": { "$ref": "common.json#/definitions/Repo" },
            "filter": { "$ref": "common.json#/definitions/IssueFilter" }
          }
        }
      }
    },

    "GitHubCreatePR": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "GitHubCreatePR" },
        "payload": {
          "type": "object",
          "required": ["repo", "title", "body", "head", "base"],
          "properties": {
            "repo": { "$ref": "common.json#/definitions/Repo" },
            "title": { "type": "string" },
            "body": { "type": "string" },
            "head": { "type": "string" },
            "base": { "type": "string" }
          }
        }
      }
    },

    "Log": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "Log" },
        "payload": {
          "type": "object",
          "required": ["level", "message"],
          "properties": {
            "level": { "enum": ["debug", "info", "warn", "error"] },
            "message": { "type": "string" },
            "fields": {
              "type": "object",
              "additionalProperties": { "type": "string" }
            }
          }
        }
      }
    },

    "DockerExec": {
      "type": "object",
      "required": ["kind", "payload"],
      "properties": {
        "kind": { "const": "DockerExec" },
        "payload": {
          "type": "object",
          "required": ["containerId", "command"],
          "properties": {
            "containerId": { "type": "string" },
            "command": {
              "type": "array",
              "items": { "type": "string" }
            },
            "workingDir": { "type": "string" }
          }
        }
      }
    },

    "EffectResult": {
      "oneOf": [
        {
          "type": "object",
          "required": ["kind", "payload"],
          "properties": {
            "kind": { "const": "Success" },
            "payload": {}
          }
        },
        {
          "type": "object",
          "required": ["kind", "payload"],
          "properties": {
            "kind": { "const": "Error" },
            "payload": {
              "type": "object",
              "required": ["message"],
              "properties": {
                "message": { "type": "string" },
                "code": { "type": "string" }
              }
            }
          }
        }
      ]
    }
  }
}
```

## Common Types Schema

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "common.json",
  "title": "Common Types",

  "definitions": {
    "Repo": {
      "type": "object",
      "required": ["owner", "name"],
      "properties": {
        "owner": { "type": "string" },
        "name": { "type": "string" }
      }
    },

    "IssueFilter": {
      "type": "object",
      "properties": {
        "state": { "enum": ["open", "closed", "all"] },
        "labels": {
          "type": "array",
          "items": { "type": "string" }
        },
        "assignee": { "type": "string" }
      }
    },

    "WorktreeInfo": {
      "type": "object",
      "required": ["path", "branch"],
      "properties": {
        "path": { "type": "string" },
        "branch": { "type": "string" },
        "commit": { "type": "string" }
      }
    }
  }
}
```

## Code Generation

### Makefile Target

```makefile
.PHONY: gen-types

gen-types: gen-types-rust gen-types-haskell

gen-types-rust:
	quicktype schema/effects.json schema/common.json \
	  --lang rust \
	  --visibility public \
	  --derive-debug \
	  --density dense \
	  --out rust/exomonad-runtime/src/generated/effects.rs

gen-types-haskell:
	quicktype schema/effects.json schema/common.json \
	  --lang haskell \
	  --module ExoMonad.Generated.Effects \
	  --out haskell/wasm-guest/src/ExoMonad/Generated/Effects.hs
```

### CI Enforcement

```yaml
# .github/workflows/schema.yml
name: Schema Sync
on: [push, pull_request]
jobs:
  check-generated:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install quicktype
        run: npm install -g quicktype
      - name: Regenerate types
        run: make gen-types
      - name: Check for drift
        run: git diff --exit-code
```

## Migration from Existing Types

Current types in:
- `rust/exomonad-shared/src/protocol.rs`
- `haskell/control-server/src/ExoMonad/Control/Protocol.hs`

Migration strategy:
1. Extract effect-related types to schema
2. Generate into new locations
3. Update imports
4. Delete old manual definitions

## Pattern: Handler Registry (from CF deploy)

The Cloudflare deploy has a clean pattern we'll adapt:

```typescript
// TypeScript version
const internalHandlers: InternalEffectHandlers<Env> = {
  LogInfo: (effect, _env) => handleLogInfo(effect),
  GitGetBranch: (effect, env) => handleGitGetBranch(effect, env),
  // ...
};
```

Rust equivalent:

```rust
// Generated trait from schema
pub trait EffectHandlers {
    async fn git_get_branch(&self, payload: GitGetBranchPayload) -> Result<String, EffectError>;
    async fn github_list_issues(&self, payload: GitHubListIssuesPayload) -> Result<Vec<Issue>, EffectError>;
    // ...
}

// Implementation
impl EffectHandlers for RuntimeServices {
    async fn git_get_branch(&self, payload: GitGetBranchPayload) -> Result<String, EffectError> {
        self.git_service.get_branch(&payload.working_dir).await
    }
    // ...
}

// Generated dispatcher
pub async fn dispatch_effect<H: EffectHandlers>(
    effect: Effect,
    handlers: &H,
) -> EffectResult {
    match effect {
        Effect::GitGetBranch { payload } => {
            handlers.git_get_branch(payload).await.into()
        }
        // ...
    }
}
```

## Open Questions

1. **Versioning**: Do we need schema versioning for plugin compatibility?
2. **Optionals**: Use `null` or omit field? Need to align aeson/serde.
3. **Numeric types**: i64 vs f64 precision for IDs?
