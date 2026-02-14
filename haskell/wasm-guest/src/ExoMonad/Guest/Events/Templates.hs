{-# LANGUAGE OverloadedStrings #-}

-- | Event template rendering.
--
-- Renders structured agent events into natural-language messages
-- suitable for injection into Claude Code sessions via Zellij.
-- These messages appear as if a user typed them, so they should
-- be clear, actionable, and parseable by the receiving LLM.
module ExoMonad.Guest.Events.Templates
  ( renderChildComplete,
    renderPRReady,
    renderCopilotReview,
    renderCIStatus,
    renderQuestionReceived,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Render a child completion event.
--
-- >>> renderChildComplete "wave0-proto" "success" "Proto files created and verified"
-- "[CHILD COMPLETE: wave0-proto] Proto files created and verified. Review their PR or check `get_agent_messages` for details."
renderChildComplete :: Text -> Text -> Text -> Text
renderChildComplete agentId status message =
  case status of
    "success" ->
      "[CHILD COMPLETE: " <> agentId <> "] "
        <> (if T.null message then "Task completed successfully." else message)
        <> " Review their PR or check `get_agent_messages` for details."
    "failure" ->
      "[CHILD FAILED: " <> agentId <> "] "
        <> (if T.null message then "Task failed." else message)
        <> " Check their worktree for details."
    _ ->
      "[CHILD STATUS: " <> agentId <> " - " <> status <> "] " <> message

-- | Render a PR ready event (Copilot approved, CI green).
renderPRReady :: Text -> Int -> Text -> Text
renderPRReady agentId prNumber prUrl =
  "[PR READY: " <> agentId <> "] PR #" <> T.pack (show prNumber)
    <> " is ready for review: " <> prUrl
    <> ". Run `merge_pr" <> T.pack (show prNumber) <> "` to merge."

-- | Render a Copilot review event.
renderCopilotReview :: Int -> Int -> Text
renderCopilotReview prNumber commentCount =
  if commentCount == 0
    then "[COPILOT APPROVED] PR #" <> T.pack (show prNumber) <> " passed Copilot review with no comments."
    else
      "[COPILOT REVIEW] PR #" <> T.pack (show prNumber)
        <> " has " <> T.pack (show commentCount)
        <> " Copilot comment" <> (if commentCount == 1 then "" else "s")
        <> ". Address them, commit, and push."

-- | Render a CI status event.
renderCIStatus :: Int -> Text -> Text -> Text
renderCIStatus prNumber status details =
  case status of
    "success" -> "[CI GREEN] PR #" <> T.pack (show prNumber) <> " passed all checks."
    "failure" -> "[CI FAILED] PR #" <> T.pack (show prNumber) <> ": " <> details
    "pending" -> "[CI PENDING] PR #" <> T.pack (show prNumber) <> " checks are running."
    _ -> "[CI " <> T.toUpper status <> "] PR #" <> T.pack (show prNumber) <> ": " <> details

-- | Render a question received event (for TL).
renderQuestionReceived :: Text -> Text -> Text -> Text
renderQuestionReceived agentId questionId question =
  "[QUESTION from " <> agentId <> ", q_id=" <> questionId <> "] "
    <> question
    <> "\nReply with `answer_question agent_id=" <> agentId
    <> " question_id=" <> questionId <> " answer=\"...\"` to respond."
