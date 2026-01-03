-- | Calendar integration effect
--
-- = ⚠️  STUB IMPLEMENTATION - Not Yet Functional
--
-- This module provides type signatures for Calendar integration
-- but all runner functions call @error@ at runtime.
--
-- __Intended use:__
--
-- * Type checking and effect composition
-- * Template for implementing real Calendar integration
--
-- __Not suitable for:__
--
-- * Production use
-- * Runtime execution (will crash with @error@)
--
-- == Implementation TODO
--
-- To make this functional, implement a real runner:
--
-- @
-- runCalendarGoogle :: GoogleCreds -> Eff (Calendar : es) a -> Eff es a
-- runCalendarGoogle creds = interpret $ \\case
--   CreateEvent title day start mEnd -> do
--     -- Call Google Calendar API
--     ...
--   ListEvents from to -> do
--     -- Call Google Calendar API
--     ...
-- @
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
import Control.Monad.Freer (Eff, Member, send, interpret)

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

data Calendar r where
  CreateEvent :: Text -> Day -> TimeOfDay -> Maybe TimeOfDay -> Calendar EventId
  ListEvents  :: Day -> Day -> Calendar [CalendarEvent]

createEvent :: Member Calendar effs => Text -> Day -> TimeOfDay -> Maybe TimeOfDay -> Eff effs EventId
createEvent title day start mEnd = send (CreateEvent title day start mEnd)

listEvents :: Member Calendar effs => Day -> Day -> Eff effs [CalendarEvent]
listEvents from to = send (ListEvents from to)

-- Stub runner (errors on call)

runCalendarStub :: Member Log effs => Eff (Calendar ': effs) a -> Eff effs a
runCalendarStub = interpret $ \case
  CreateEvent title day start _ -> do
    logInfo $ "[Calendar:stub] CreateEvent called: " <> title <> " on " <> T.pack (show day) <> " at " <> T.pack (show start)
    error "Calendar.createEvent: not implemented"
  ListEvents from to -> do
    logInfo $ "[Calendar:stub] ListEvents called: " <> T.pack (show from) <> " to " <> T.pack (show to)
    error "Calendar.listEvents: not implemented"
