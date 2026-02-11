module ExoMonad.Guest.Records.Events
  ( EventTools (..),
    eventTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, ToolMode ((:-)), mkHandler)
import ExoMonad.Guest.Tools.Events (WaitForEvent, NotifyCompletion)
import GHC.Generics (Generic)

data EventTools mode = EventTools
  { waitForEvent :: mode :- WaitForEvent,
    notifyCompletion :: mode :- NotifyCompletion
  }
  deriving (Generic)

eventTools :: EventTools AsHandler
eventTools =
  EventTools
    { waitForEvent = mkHandler @WaitForEvent,
      notifyCompletion = mkHandler @NotifyCompletion
    }
