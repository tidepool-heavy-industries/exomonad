{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Coordination tool record.
module ExoMonad.Guest.Records.Coordination
  ( CoordinationTools (..),
    coordinationToolsHandler,
    coordinationToolsSchema,
    coordinationTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Coordination
import GHC.Generics (Generic)

-- | Coordination tools record.
data CoordinationTools mode = CoordinationTools
  { coordCreateTask :: mode :- CreateTask,
    coordUpdateTask :: mode :- UpdateTask,
    coordListTasks :: mode :- ListTasks,
    coordGetTask :: mode :- GetTask,
    coordSendMessage :: mode :- SendCoordMessage,
    coordGetMessages :: mode :- GetCoordMessages
  }
  deriving (Generic)

-- | Coordination tools handler record.
coordinationToolsHandler :: CoordinationTools AsHandler
coordinationToolsHandler =
  CoordinationTools
    { coordCreateTask = mkHandler @CreateTask,
      coordUpdateTask = mkHandler @UpdateTask,
      coordListTasks = mkHandler @ListTasks,
      coordGetTask = mkHandler @GetTask,
      coordSendMessage = mkHandler @SendCoordMessage,
      coordGetMessages = mkHandler @GetCoordMessages
    }

-- | Coordination tools schema record.
coordinationToolsSchema :: CoordinationTools AsSchema
coordinationToolsSchema =
  CoordinationTools
    { coordCreateTask = mkSchema @CreateTask,
      coordUpdateTask = mkSchema @UpdateTask,
      coordListTasks = mkSchema @ListTasks,
      coordGetTask = mkSchema @GetTask,
      coordSendMessage = mkSchema @SendCoordMessage,
      coordGetMessages = mkSchema @GetCoordMessages
    }

-- | Default handler instance for use in Role.hs
coordinationTools :: CoordinationTools AsHandler
coordinationTools = coordinationToolsHandler