# GitHub Interpreter - gh CLI Integration

Interprets the `GitHub` effect by calling the `gh` CLI tool for issue and PR operations.

## When to Read This

Read this if you're:
- Building agents that interact with GitHub issues/PRs
- Understanding how agents fetch issue context
- Debugging gh CLI authentication issues
- Working with urchin's issue tracking

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   listIssues (Repo "owner/repo") filter                             │
│   getIssue (Repo "owner/repo") 123 includeComments                  │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ GitHub effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ GitHub Interpreter (subprocess)                                         │
│   gh issue list --repo owner/repo --json ...                        │
│   gh issue view 123 --repo owner/repo --json ...                    │
│   gh pr list --repo owner/repo --json ...                           │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ JSON stdout
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ GitHub API                                                           │
│   (authenticated via gh auth login)                                  │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Effects.GitHub

main :: IO ()
main = runM $ runGitHubIO defaultGitHubConfig $ do
  -- List open issues
  issues <- listIssues (Repo "owner/repo") defaultIssueFilter

  -- Get specific issue with comments
  issue <- getIssue (Repo "owner/repo") 123 True

  -- List pull requests
  prs <- listPullRequests (Repo "owner/repo") defaultPRFilter

  pure (issues, issue, prs)
```

## Effect Operations

### Issues

| Operation | CLI Command | Returns |
|-----------|-------------|---------|
| `listIssues repo filter` | `gh issue list --json ...` | `[Issue]` |
| `getIssue repo num comments` | `gh issue view N --json ...` | `Maybe Issue` |

### Pull Requests

| Operation | CLI Command | Returns |
|-----------|-------------|---------|
| `listPullRequests repo filter` | `gh pr list --json ...` | `[PullRequest]` |
| `getPullRequest repo num` | `gh pr view N --json ...` | `Maybe PullRequest` |

## Filters

```haskell
data IssueFilter = IssueFilter
  { ifState :: Maybe IssueState   -- Open, Closed, All
  , ifLabels :: [Text]            -- Filter by labels
  , ifLimit :: Maybe Int          -- Max results
  }

data PRFilter = PRFilter
  { prfState :: Maybe PRState     -- Open, Closed, Merged, All
  , prfLimit :: Maybe Int
  }

defaultIssueFilter :: IssueFilter
defaultPRFilter :: PRFilter
```

## Configuration

```haskell
data GitHubConfig = GitHubConfig
  { ghcQuiet :: Bool  -- Suppress stderr warnings
  }

defaultGitHubConfig :: GitHubConfig
defaultGitHubConfig = GitHubConfig { ghcQuiet = True }
```

## Requirements

The `gh` CLI must be installed and authenticated:

```bash
# One-time setup
gh auth login

# Verify
gh auth status
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, CLI subprocess calls |
| `test/Main.hs` | Integration tests (require gh auth) |

## Error Handling

CLI errors are returned as exceptions in IO. Wrap in try for recovery:

```haskell
result <- try @SomeException $ runM $ runGitHubIO config $ do
  getIssue repo num False
case result of
  Left err -> handleError err
  Right issue -> useIssue issue
```

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [effects/bd-interpreter/CLAUDE.md](../bd-interpreter/CLAUDE.md) - Beads task tracking (local)
