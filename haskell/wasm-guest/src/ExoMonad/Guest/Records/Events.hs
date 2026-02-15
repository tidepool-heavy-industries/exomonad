module ExoMonad.Guest.Records.Events
  ( EventTools (..),
    eventTools,
    NotifyParent,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, ToolMode ((:-)), mkHandler)
import ExoMonad.Guest.Tools.Events (NotifyParent, WaitForEvent)
import GHC.Generics (Generic)

data EventTools mode = EventTools
  { waitForEvent :: mode :- WaitForEvent
  }
  deriving (Generic)

eventTools :: EventTools AsHandler
eventTools =
  EventTools
    { waitForEvent = mkHandler @WaitForEvent
    }
