{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Teams task management effects for Gemini agents.
--
-- All effects are dispatched via the @teams@ namespace.
-- Request and response types are proto-generated from @proto/effects/teams.proto@.
module ExoMonad.Effects.Teams
  ( -- * Effect Types
    TeamsClaimTask,
    TeamsCompleteTask,
    TeamsListTasks,
    TeamsGetTask,
    TeamsReportStatus,
    TeamsAskQuestion,

    -- * Smart Constructors
    claimTask,
    completeTask,
    listTasks,
    getTask,
    reportStatus,
    askQuestion,

    -- * Re-exported proto types
    module Effects.Teams,
  )
where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.EffectError (EffectError)
import Effects.Teams
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data TeamsClaimTask

instance Effect TeamsClaimTask where
  type Input TeamsClaimTask = ClaimTaskRequest
  type Output TeamsClaimTask = ClaimTaskResponse
  effectId = "teams.claim_task"

data TeamsCompleteTask

instance Effect TeamsCompleteTask where
  type Input TeamsCompleteTask = CompleteTaskRequest
  type Output TeamsCompleteTask = CompleteTaskResponse
  effectId = "teams.complete_task"

data TeamsListTasks

instance Effect TeamsListTasks where
  type Input TeamsListTasks = ListTasksRequest
  type Output TeamsListTasks = ListTasksResponse
  effectId = "teams.list_tasks"

data TeamsGetTask

instance Effect TeamsGetTask where
  type Input TeamsGetTask = GetTaskRequest
  type Output TeamsGetTask = GetTaskResponse
  effectId = "teams.get_task"

data TeamsReportStatus

instance Effect TeamsReportStatus where
  type Input TeamsReportStatus = ReportStatusRequest
  type Output TeamsReportStatus = ReportStatusResponse
  effectId = "teams.report_status"

data TeamsAskQuestion

instance Effect TeamsAskQuestion where
  type Input TeamsAskQuestion = AskQuestionRequest
  type Output TeamsAskQuestion = AskQuestionResponse
  effectId = "teams.ask_question"

-- ============================================================================
-- Smart constructors
-- ============================================================================

claimTask :: Text -> IO (Either EffectError ClaimTaskResponse)
claimTask taskId =
  runEffect @TeamsClaimTask $
    ClaimTaskRequest
      { claimTaskRequestTaskId = TL.fromStrict taskId
      }

completeTask :: Text -> Text -> IO (Either EffectError CompleteTaskResponse)
completeTask taskId summary =
  runEffect @TeamsCompleteTask $
    CompleteTaskRequest
      { completeTaskRequestTaskId = TL.fromStrict taskId,
        completeTaskRequestSummary = TL.fromStrict summary
      }

listTasks :: IO (Either EffectError ListTasksResponse)
listTasks =
  runEffect @TeamsListTasks ListTasksRequest

getTask :: Text -> IO (Either EffectError GetTaskResponse)
getTask taskId =
  runEffect @TeamsGetTask $
    GetTaskRequest
      { getTaskRequestTaskId = TL.fromStrict taskId
      }

reportStatus :: Text -> IO (Either EffectError ReportStatusResponse)
reportStatus content =
  runEffect @TeamsReportStatus $
    ReportStatusRequest
      { reportStatusRequestContent = TL.fromStrict content
      }

askQuestion :: Text -> IO (Either EffectError AskQuestionResponse)
askQuestion question =
  runEffect @TeamsAskQuestion $
    AskQuestionRequest
      { askQuestionRequestQuestion = TL.fromStrict question
      }
