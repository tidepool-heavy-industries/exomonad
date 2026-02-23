# GitHub Interpreter - Socket Client (Rust/Octocrab)

Interprets the `GitHub` effect by communicating with the Rust `exomonad-core` backend via Unix Domain Socket. The Rust side uses Octocrab for REST and GraphQL queries against the GitHub API.

## When to Read This

Read this if you're:
- Building agents that interact with GitHub issues/PRs
- Understanding how agents fetch issue context
- Debugging GitHub API or socket communication issues
- Working with urchin's issue tracking

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   listIssues (Repo "owner/repo") filter                             │
│   getIssue (Repo "owner/repo") 123 includeComments                  │
│   getPullRequest (Repo "owner/repo") 99 includeDetails              │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ GitHub effect (freer-simple)
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ GitHub Interpreter (Haskell)                                         │
│   Encodes ServiceRequest → JSON                                      │
│   Sends over Unix Domain Socket (NDJSON)                             │
│   Decodes ServiceResponse ← JSON                                     │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Unix Socket (NDJSON)
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Rust Service (exomonad-core)                                         │
│   Octocrab REST + GraphQL → GitHub API                               │
│   (authenticated via GITHUB_TOKEN)                                   │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.Effects.GitHub

main :: IO ()
main = runM $ runGitHubIO defaultGitHubConfig $ do
  -- List open issues
  issues <- listIssues (Repo "owner/repo") defaultIssueFilter

  -- Get specific issue with comments
  issue <- getIssue (Repo "owner/repo") 123 True

  -- List pull requests
  prs <- listPullRequests (Repo "owner/repo") defaultPRFilter

  -- Get PR with full details (comments, reviews, labels, merged_at)
  pr <- getPullRequest (Repo "owner/repo") 99 True

  pure (issues, issue, prs, pr)
```

## Effect Operations

### Issues

| Operation | Wire Type | Returns |
|-----------|-----------|---------|
| `listIssues repo filter` | `GitHubListIssues` | `[Issue]` |
| `getIssue repo num includeComments` | `GitHubGetIssue` | `Maybe Issue` |
| `createIssue repo spec` | `GitHubCreateIssue` | `Issue` |
| `updateIssue repo num spec` | `GitHubUpdateIssue` | `Issue` |

### Pull Requests

| Operation | Wire Type | Returns |
|-----------|-----------|---------|
| `listPullRequests repo filter` | `GitHubListPullRequests` | `[PullRequest]` |
| `getPullRequest repo num includeDetails` | `GitHubGetPR` | `Maybe PullRequest` |
| `createPR repo spec` | `GitHubCreatePR` | `PullRequest` |

### Flags

- `includeComments` (`GitHubGetIssue`): When true, issue response includes comments from the Rust service.
- `includeDetails` (`GitHubGetPR`): When true, PR response includes comments, review threads (via GraphQL), labels, and `merged_at`.

## Configuration

```haskell
data GitHubConfig = GitHubConfig
  { ghcSocketPath :: FilePath  -- Unix socket path to Rust service
  }

defaultGitHubConfig :: GitHubConfig
```

The socket path defaults to `EXOMONAD_CONTROL_SOCKET` or `.exo/sockets/control.sock`.

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, socket communication, JSON parsing |
| `test/Main.hs` | Integration tests (JSON parsing, no network required) |

## Wire Format

The interpreter communicates using the `ServiceRequest`/`ServiceResponse` types defined in `exomonad-socket-client`. These must match the Rust `protocol.rs` types exactly. Key response fields:

**GitHubIssueResponse**: `number`, `title`, `body`, `state`, `labels`, `url`, `author`, `comments`

**GitHubPRResponse**: `number`, `title`, `body`, `author`, `url`, `state`, `head_ref_name`, `base_ref_name`, `created_at`, `merged_at`, `labels`, `comments`, `reviews`

## Error Handling

Socket/decode errors are returned as `GitHubError` values. The interpreter handles:
- `SocketError` — connection failures
- `DecodeError` — JSON parsing failures
- `TimeoutError` — socket timeout
- `ErrorResponse` — GitHub API errors forwarded from Rust

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- `rust/exomonad-core/src/protocol/` - Wire format types (Rust side)
- `haskell/effects/socket-client/` - Socket client and protocol types (Haskell side)
