# Git Integration Research

> **STATUS: RESEARCH / REFERENCE**
> This document represents early research. Production implementations may differ.
> See `haskell/effects/worktree-interpreter/CLAUDE.md` or `rust/effector/CLAUDE.md` for current git handling.

> Research findings for the Git effect in exomonad.

## Recommendation: Use Git CLI

**Recommended approach**: Shell out to git CLI with structured formats.

**Why not libgit2 bindings?**
- [hlibgit2](https://hackage.haskell.org/package/hlibgit2): Binds to old libgit2 v0.18.0
- [gitlib](https://hackage.haskell.org/package/gitlib): Last updated 2021
- Maintenance burden for FFI bindings
- CLI is stable, well-documented, always up-to-date

**Benefits of CLI approach**:
- No native dependencies
- Easy to test with mock outputs

## Command Output Formats

### git log

**Recommended format**: Pipe-delimited with controlled fields

```bash
git log --format="%H|%an|%ae|%at|%s" -n 10
```

Output:
```
292faaa2904cd5dc915a78ca478b3edd8e5af406|toast|inanna@recursion.wtf|1767520887|feat(github-effect): Add GitHub effect
c6514f5af16fa1cc610389eaada619cec0d3fa94|toast|inanna@recursion.wtf|1767520058|docs: Add Gas Town LSP integration
```

**Format placeholders**:
| Placeholder | Description |
|-------------|-------------|
| `%H` | Full commit hash |
| `%h` | Short commit hash |
| `%an` | Author name |
| `%ae` | Author email |
| `%at` | Author timestamp (unix) |
| `%ct` | Committer timestamp (unix) |
| `%s` | Subject (first line) |
| `%b` | Body (remaining lines) |

**Parsing approach**:
```haskell
parseLogLine :: Text -> Maybe CommitInfo
parseLogLine line = case T.splitOn "|" line of
  [hash, author, email, timestamp, subject] ->
    Just CommitInfo
      {
        ciHash = hash
      ,
        ciAuthor = author
      ,
        ciEmail = email
      ,
        ciTimestamp = read (T.unpack timestamp)
      ,
        ciSubject = subject
      }
  _ -> Nothing
```

**Caveat**: Subject may contain `|` characters. For robustness, use `%x00` (null byte) as delimiter:

```bash
git log --format="%H%x00%an%x00%ae%x00%at%x00%s" -n 10
```

### git diff

**For file lists**: `--name-only`
```bash
git diff --name-only HEAD~3..HEAD
```

Output:
```
CLAUDE.md
cabal.project
docs/gas-town-lsp-integration.md
```

**With status**: `--name-status`
```bash
git diff --name-status HEAD~3..HEAD
```

Output:
```
M	CLAUDE.md
M	cabal.project
A	docs/gas-town-lsp-integration.md
```

Status codes:
| Code | Meaning |
|------|---------|
| `A` | Added |
| `D` | Deleted |
| `M` | Modified |
| `R` | Renamed |
| `C` | Copied |

**Parsing approach**:
```haskell
data FileChange = FileChange
  {
    fcStatus :: ChangeStatus
  ,
    fcPath :: FilePath
  }

parseNameStatus :: Text -> Maybe FileChange
parseNameStatus line = case T.splitOn "\t" line of
  [status, path] -> Just FileChange
    {
      fcStatus = parseStatus status
    ,
      fcPath = T.unpack path
    }
  _ -> Nothing
```

### git blame

**Porcelain format** (structured, stable):
```bash
git blame --porcelain -L1,10 file.hs
```

Output structure:
```
<sha> <orig-line> <final-line> <num-lines>
author <name>
author-mail <email>
author-time <timestamp>
author-tz <timezone>
committer <name>
committer-mail <email>
committer-time <timestamp>
committer-tz <timezone>
summary <commit message>
[previous <sha> <filename>]
[boundary]
filename <filename>
	<line-content>
```

**Parsing approach**: State machine parsing
1. Read SHA line (starts blame block)
2. Read metadata lines until `filename`
3. Read content line (starts with tab)
4. Repeat

```haskell
data BlameLine = BlameLine
  {
    blCommit :: Text
  ,
    blAuthor :: Text
  ,
    blAuthorTime :: Int
  ,
    blLineNum :: Int
  ,
    blContent :: Text
  }
```

### git show

**For commit details with stats**:
```bash
git show --stat --format="%H%n%an%n%ae%n%at%n%s%n%b" HEAD
```

Output:
```
292faaa2904cd5dc915a78ca478b3edd8e5af406
theast
inanna@recursion.wtf
1767520887
feat(github-effect): Add GitHub effect for reading issue info

Add effect for querying GitHub API...

 cabal.project | 1 +
 haskell/dsl/core/src/ExoMonad/Effects/GitHub.hs | 268 ++++++++++++++++++
 5 files changed, 530 insertions(+)
```

**Note**: The stat output format is complex. For programmatic use, prefer `--numstat`:

```bash
git show --numstat --format="" HEAD
```

Output:
```
1	0	cabal.project
268	0	haskell/dsl/core/src/ExoMonad/Effects/GitHub.hs
```

Format: `<additions>\t<deletions>\t<path>`

## Git Effect Types

Based on the research, here are the recommended types:

```haskell
-- | Git effect for repository queries.
data Git :: Effect where
  -- | Get recent commits
  GetLog :: Int -> Git m [CommitInfo]

  -- | Get changed files between refs
  GetDiff :: Text -> Text -> Git m [FileChange]

  -- | Get blame info for file region
  GetBlame :: FilePath -> Int -> Int -> Git m [BlameLine]

  -- | Get commit details
  GetCommit :: Text -> Git m (Maybe CommitDetails)

-- | Commit summary info (from git log)
data CommitInfo = CommitInfo
  {
    ciHash :: Text
  ,
    ciAuthor :: Text
  ,
    ciEmail :: Text
  ,
    ciTimestamp :: Int
  ,
    ciSubject :: Text
  }

-- | File change info (from git diff)
data FileChange = FileChange
  {
    fcStatus :: ChangeStatus
  ,
    fcPath :: FilePath
  ,
    fcAdditions :: Maybe Int  -- from --numstat
  ,
    fcDeletions :: Maybe Int
  }

data ChangeStatus = Added | Modified | Deleted | Renamed | Copied

-- | Blame line info (from git blame --porcelain)
data BlameLine = BlameLine
  {
    blCommit :: Text
  ,
    blAuthor :: Text
  ,
    blAuthorTime :: Int
  ,
    blLineNum :: Int
  ,
    blContent :: Text
  }

-- | Full commit details (from git show)
data CommitDetails = CommitDetails
  {
    cdHash :: Text
  ,
    cdAuthor :: Text
  ,
    cdEmail :: Text
  ,
    cdTimestamp :: Int
  ,
    cdSubject :: Text
  ,
    cdBody :: Text
  ,
    cdFiles :: [FileChange]
  }
```

## Interpreter Implementation

Follow the standard interpreter pattern:

```haskell
-- | Run Git effects using git CLI.
runGitIO :: LastMember IO effs => GitConfig -> Eff (Git ': effs) a -> Eff effs a
runGitIO config = interpret $ \case
  GetLog n -> sendM $ gitLog config n
  GetDiff from to -> sendM $ gitDiff config from to
  GetBlame path start end -> sendM $ gitBlame config path start end
  GetCommit ref -> sendM $ gitShow config ref

-- | Configuration for Git interpreter.
data GitConfig = GitConfig
  {
    gcRepoPath :: Maybe FilePath  -- If Nothing, use cwd
  ,
    gcQuiet :: Bool               -- Suppress stderr
  }
```

## Large Repo Efficiency

**Concerns**:
- `git log` on large repos can be slow without `-n` limit
- `git blame` on large files is slow
- `git diff` between distant refs can be expensive

**Mitigations**:
- Always use `-n` limit for log queries
- Use `-L start,end` for blame (line range)
- Cache results when possible
- Use `--no-walk` for single commit queries

## Alternative: Simple Process Wrapper

For minimal implementation, a simple process wrapper suffices:

```haskell
import System.Process (readProcessWithExitCode)

gitCmd :: [String] -> IO (Either Text Text)
gitCmd args = do
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  case code of
    ExitSuccess -> Right (T.pack stdout)
    ExitFailure _ -> Left (T.pack stderr)
```

This matches the standard interpreter pattern and is sufficient for chiton-spawn context construction.

## References

- [Git Log Format Placeholders](https://git-scm.com/docs/git-log#_pretty_formats)
- [Git Diff Options](https://git-scm.com/docs/git-diff)
- [Git Blame Porcelain](https://git-scm.com/docs/git-blame#_the_porcelain_format)
- [hlibgit2 (Hackage)](https://hackage.haskell.org/package/hlibgit2) - libgit2 bindings (outdated)
- [gitlib (Hackage)](https://hackage.haskell.org/package/gitlib) - High-level git API (last updated 2021)
