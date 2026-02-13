module ExoMonad.Guest.Records.Events
  ( EventTools (..),
    eventTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, ToolMode ((:-)), mkHandler)
import ExoMonad.Guest.Tools.Events (WaitForEvent, NotifyParent)
import GHC.Generics (Generic)

data EventTools mode = EventTools
  { waitForEvent :: mode :- WaitForEvent,
    notifyParent :: mode :- NotifyParent
  }
  deriving (Generic)

eventTools :: EventTools AsHandler
eventTools =
  EventTools
    { waitForEvent = mkHandler @WaitForEvent,
      notifyParent = mkHandler @NotifyParent
    }
