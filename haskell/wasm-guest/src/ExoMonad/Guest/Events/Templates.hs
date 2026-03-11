{-# LANGUAGE OverloadedStrings #-}

-- | Centralized message templates for event handler notifications.
--
-- Every structured message that event handlers produce lives here as a typed
-- function. This makes the message format discoverable and consistent.
--
-- These messages are delivered to the parent via @notify_parent_delivery@,
-- which prepends @[from: agent-id]@ automatically. The structural tags
-- (@[PR READY]@, @[REVIEW TIMEOUT]@, etc.) are the actionable signal
-- the TL pattern-matches on.
module ExoMonad.Guest.Events.Templates
  ( prReady,
    reviewTimeout,
    fixesPushed,
    commitsPushed,
    copilotReviewReceived,
    siblingMerged,
    ciStatus,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | PR approved by Copilot — signals TL to merge.
--
-- >>> prReady 42
-- "[PR READY] PR #42 approved by Copilot review. Merge with `merge_pr` tool."
prReady :: Int -> Text
prReady n =
  "[PR READY] PR #" <> T.pack (show n)
    <> " approved by Copilot review. Merge with `merge_pr` tool."

-- | No Copilot review within timeout — signals TL to merge if CI passes.
--
-- >>> reviewTimeout 42 15
-- "[REVIEW TIMEOUT] PR #42 \x2014 no Copilot review after 15 minutes. Merge with `merge_pr` using `force: true`."
reviewTimeout :: Int -> Int -> Text
reviewTimeout n mins =
  "[REVIEW TIMEOUT] PR #" <> T.pack (show n)
    <> " \x2014 no Copilot review after " <> T.pack (show mins)
    <> " minutes. Merge with `merge_pr` using `force: true`."

-- | Fixes pushed after Copilot review — Copilot does NOT re-review,
-- so this is the actionable signal for the TL.
--
-- >>> fixesPushed 42 "success"
-- "[FIXES PUSHED] PR #42 \x2014 review comments addressed, fixes pushed. CI passing. Ready to merge."
fixesPushed :: Int -> Text -> Text
fixesPushed n ci =
  "[FIXES PUSHED] PR #" <> T.pack (show n)
    <> " \x2014 review comments addressed, fixes pushed."
    <> case ci of
         "success" -> " CI passing. Ready to merge."
         "pending" -> " CI running \x2014 merge when green."
         _ -> " CI status: " <> ci <> "."

-- | Copilot posted review comments — injected into the agent's pane.
--
-- >>> copilotReviewReceived 42 "Fix the typo on line 3."
-- "## Copilot Review on PR #42\n\nFix the typo on line 3.\n\nAddress these comments and push fixes."
copilotReviewReceived :: Int -> Text -> Text
copilotReviewReceived n comments =
  "## Copilot Review on PR #" <> T.pack (show n) <> "\n\n"
    <> comments
    <> "\n\nAddress these comments and push fixes."

-- | A sibling branch was merged — injected into the agent's pane with rebase instructions.
--
-- >>> siblingMerged "main.feature-a" "main"
-- "[Sibling Merged] PR on branch main.feature-a was merged into main. Rebase your branch to pick up the changes: git fetch origin && git rebase origin/main"
siblingMerged :: Text -> Text -> Text
siblingMerged mergedBranch parentBranch =
  "[Sibling Merged] PR on branch " <> mergedBranch
    <> " was merged into " <> parentBranch
    <> ". Rebase your branch to pick up the changes: git fetch origin && git rebase origin/" <> parentBranch

-- | CI status changed — injected into the agent's pane.
--
-- >>> ciStatus 42 "success" "main.feature-a"
-- "[CI Status] PR #42 on branch main.feature-a: success\n\nCI passed."
ciStatus :: Int -> Text -> Text -> Text
ciStatus n status branch =
  "[CI Status] PR #" <> T.pack (show n) <> " on branch " <> branch
    <> ": " <> status
    <> case status of
         "success" -> "\n\nCI passed."
         "failure" -> "\n\nCI failed. Check the logs and fix the issue before proceeding."
         _ -> ""

-- | New commits pushed to a PR — informational notification to parent.
--
-- >>> commitsPushed 42 "success"
-- "[COMMITS PUSHED] PR #42 \x2014 new commits pushed. CI passing."
commitsPushed :: Int -> Text -> Text
commitsPushed n ci =
  "[COMMITS PUSHED] PR #" <> T.pack (show n)
    <> " \x2014 new commits pushed."
    <> case ci of
         "success" -> " CI passing."
         "pending" -> " CI running."
         "failure" -> " CI failing."
         _ -> " CI status: " <> ci <> "."
