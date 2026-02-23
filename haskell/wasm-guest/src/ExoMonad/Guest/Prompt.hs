-- | Pure prompt builder for composing worker/leaf agent prompts.
--
-- Replaces the template effect (Haskell → proto → Rust → disk → proto → Haskell)
-- with direct string assembly in Haskell. No I/O needed.
module ExoMonad.Guest.Prompt
  ( Prompt,
    render,
    task,
    boundary,
    steps,
    context,
    contextFile,
    verify,
    doneCriteria,
    readFirst,
    raw,
    leafProfile,
    workerProfile,
    generalProfile,
    rustProfile,
    haskellProfile,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | A prompt is a sequence of sections, composed via '<>'.
newtype Prompt = Prompt [Section]
  deriving (Semigroup, Monoid)

data Section
  = Headed Text Text -- heading, body
  | Raw Text -- no heading

-- | Render a prompt to markdown text. Skips empty sections.
render :: Prompt -> Text
render (Prompt sections) =
  T.strip $ T.intercalate "\n\n" $ filter (not . T.null) $ map renderSection sections
  where
    renderSection (Headed h b)
      | T.null (T.strip b) = ""
      | otherwise = "## " <> h <> "\n" <> b
    renderSection (Raw t) = t

-- | Task section.
task :: Text -> Prompt
task t = Prompt [Headed "TASK" t]

-- | Boundary section (bulleted list of DO NOT rules).
boundary :: [Text] -> Prompt
boundary [] = mempty
boundary items = Prompt [Headed "BOUNDARY" $ T.intercalate "\n" $ map ("- " <>) items]

-- | Steps section (numbered list).
steps :: [Text] -> Prompt
steps [] = mempty
steps items =
  Prompt
    [ Headed "STEPS" $
        T.intercalate "\n" $
          zipWith (\i s -> T.pack (show (i :: Int)) <> ". " <> s) [1 ..] items
    ]

-- | Freeform context section.
context :: Text -> Prompt
context "" = mempty
context t = Prompt [Headed "CONTEXT" t]

-- | Context file section with path and content.
contextFile :: Text -> Text -> Prompt
contextFile path content =
  Prompt [Headed ("CONTEXT: File: `" <> path <> "`") $ "```\n" <> content <> ensureNewline content <> "```"]
  where
    ensureNewline t
      | T.null t = "\n"
      | T.last t == '\n' = ""
      | otherwise = "\n"

-- | Verify section (backtick-wrapped commands).
verify :: [Text] -> Prompt
verify [] = mempty
verify items = Prompt [Headed "VERIFY" $ T.intercalate "\n" $ map (\c -> "- `" <> c <> "`") items]

-- | Done criteria section (bulleted).
doneCriteria :: [Text] -> Prompt
doneCriteria [] = mempty
doneCriteria items = Prompt [Headed "DONE CRITERIA" $ T.intercalate "\n" $ map ("- " <>) items]

-- | Read first section (bulleted paths).
readFirst :: [Text] -> Prompt
readFirst [] = mempty
readFirst items = Prompt [Headed "READ FIRST" $ T.intercalate "\n" $ map ("- " <>) items]

-- | Raw text with no heading (escape hatch).
raw :: Text -> Prompt
raw "" = mempty
raw t = Prompt [Raw t]

-- ============================================================================
-- Inline profiles (moved from .exo/templates/profiles/*.md)
-- ============================================================================

-- | Completion protocol for leaf subtree agents.
leafProfile :: Prompt
leafProfile =
  Prompt
    [ Headed "Completion Protocol (Leaf Subtree)" $
        T.intercalate
          "\n"
          [ "You are a **leaf agent** in your own git worktree and branch. Your branch name follows the pattern `{parent}.{slug}`.",
            "",
            "When you are done:",
            "",
            "1. **Commit your changes** with a descriptive message.",
            "   - `git add <specific files>` — NEVER `git add .` or `git add -A`",
            "   - `git commit -m \"feat: <description>\"`",
            "2. **File a PR** using `file_pr` tool. The base branch is auto-detected from your branch name.",
            "3. **Wait for Copilot review** if it arrives. Address review comments, push fixes.",
            "4. **Call `notify_parent`** with status `success`, a one-line summary, and the PR number.",
            "   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.",
            "",
            "**DO NOT:**",
            "- Merge your own PR (the parent TL merges)",
            "- Push to main or any branch other than your own",
            "- Create additional branches"
          ]
    ]

-- | Completion protocol for ephemeral worker agents.
workerProfile :: Prompt
workerProfile =
  Prompt
    [ Headed "Completion Protocol (Worker)" $
        T.intercalate
          "\n"
          [ "You are an **ephemeral worker** — you run in the parent's directory on the parent's branch. You do NOT have your own worktree or branch.",
            "",
            "When you are done:",
            "",
            "1. **Commit your changes** to the current branch with a descriptive message.",
            "   - `git add <specific files>` — NEVER `git add .` or `git add -A`",
            "   - `git commit -m \"feat: <description>\"`",
            "2. **Call `notify_parent`** with status `success` and a one-line summary of what you accomplished.",
            "   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.",
            "",
            "**DO NOT:**",
            "- File a PR (you have no branch to PR from)",
            "- Push to remote (you're on the parent's branch)",
            "- Create new branches",
            "- Run `git checkout` or `git switch`"
          ]
    ]

-- | General anti-patterns for all agents.
generalProfile :: Prompt
generalProfile =
  boundary
    [ "Do NOT use `git add .` or `git add -A`. Only `git add <specific files>`.",
      "Do NOT add trailing whitespace to any line.",
      "Do NOT add `todo!()`, `unimplemented!()`, or `unreachable!()`.",
      "Do NOT add stream-of-consciousness comments. Doc comments only where non-obvious.",
      "Do NOT rename existing types, modules, or functions unless instructed.",
      "Do NOT modify files not listed in the spec."
    ]

-- | Rust-specific anti-patterns.
rustProfile :: Prompt
rustProfile =
  boundary
    [ "Do NOT add unnecessary crate dependencies.",
      "Do NOT use `unsafe` blocks unless explicitly required.",
      "Use `?` operator for error propagation, not `.unwrap()`."
    ]

-- | Haskell-specific anti-patterns.
haskellProfile :: Prompt
haskellProfile =
  boundary
    [ "Do NOT strip `-` from LANGUAGE pragma closings. The correct form is `#-}`, NOT `#}`.",
      "Do NOT break lambda syntax or string literals.",
      "Do NOT add unnecessary LANGUAGE extensions.",
      "Run `sed -i 's/[[:space:]]*$//' <file>` after editing to remove trailing whitespace."
    ]
