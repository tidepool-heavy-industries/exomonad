-- | Calendar integration effect
module Tidepool.Effects.Calendar
  ( -- * Effect
    Calendar(..)
  , createEvent
  , listEvents

    -- * Types
  , CalendarEvent(..)
  , EventId(..)

    -- * Runner (stub)
  , runCalendarStub
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, Day, TimeOfDay)
import GHC.Generics (Generic)
import Effectful
import Effectful.Dispatch.Dynamic

import Tidepool.Effect (Log, logInfo)

-- Types

newtype EventId = EventId { unEventId :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data CalendarEvent = CalendarEvent
  { eventId    :: EventId
  , eventTitle :: Text
  , eventStart :: UTCTime
  , eventEnd   :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Effect

data Calendar :: Effect where
  CreateEvent :: Text -> Day -> TimeOfDay -> Maybe TimeOfDay -> Calendar m EventId
  ListEvents  :: Day -> Day -> Calendar m [CalendarEvent]

type instance DispatchOf Calendar = 'Dynamic

createEvent :: Calendar :> es => Text -> Day -> TimeOfDay -> Maybe TimeOfDay -> Eff es EventId
createEvent title day start mEnd = send (CreateEvent title day start mEnd)

listEvents :: Calendar :> es => Day -> Day -> Eff es [CalendarEvent]
listEvents from to = send (ListEvents from to)

-- Stub runner (errors on call)

runCalendarStub :: (IOE :> es, Log :> es) => Eff (Calendar : es) a -> Eff es a
runCalendarStub = interpret $ \_ -> \case
  CreateEvent title day start _ -> do
    logInfo $ "[Calendar:stub] CreateEvent called: " <> title <> " on " <> T.pack (show day) <> " at " <> T.pack (show start)
    error "Calendar.createEvent: not implemented"
  ListEvents from to -> do
    logInfo $ "[Calendar:stub] ListEvents called: " <> T.pack (show from) <> " to " <> T.pack (show to)
    error "Calendar.listEvents: not implemented"
