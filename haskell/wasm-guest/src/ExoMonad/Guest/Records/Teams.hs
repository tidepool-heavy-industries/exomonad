-- | Teams tool record and handlers.
--
-- Tools for Gemini agents to participate in Claude Code Teams task management.
module ExoMonad.Guest.Records.Teams
  ( TeamsTools (..),
    teamsToolsHandler,
    teamsToolsSchema,
    teamsTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Teams (AskQuestion, ClaimTask, CompleteTask, GetTask, ListTasks, ReportStatus)
import GHC.Generics (Generic)

-- | Teams tools record.
data TeamsTools mode = TeamsTools
  { claimTask :: mode :- ClaimTask,
    completeTask :: mode :- CompleteTask,
    listTasks :: mode :- ListTasks,
    getTask :: mode :- GetTask,
    reportStatus :: mode :- ReportStatus,
    askQuestion :: mode :- AskQuestion
  }
  deriving (Generic)

-- | Teams tools handler record.
teamsToolsHandler :: TeamsTools AsHandler
teamsToolsHandler =
  TeamsTools
    { claimTask = mkHandler @ClaimTask,
      completeTask = mkHandler @CompleteTask,
      listTasks = mkHandler @ListTasks,
      getTask = mkHandler @GetTask,
      reportStatus = mkHandler @ReportStatus,
      askQuestion = mkHandler @AskQuestion
    }

-- | Teams tools schema record.
teamsToolsSchema :: TeamsTools AsSchema
teamsToolsSchema =
  TeamsTools
    { claimTask = mkSchema @ClaimTask,
      completeTask = mkSchema @CompleteTask,
      listTasks = mkSchema @ListTasks,
      getTask = mkSchema @GetTask,
      reportStatus = mkSchema @ReportStatus,
      askQuestion = mkSchema @AskQuestion
    }

-- | Default handler instance for use in Role.hs.
teamsTools :: TeamsTools AsHandler
teamsTools = teamsToolsHandler
