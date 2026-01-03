-- | Type-safe Habitica API types and operations.
--
-- This module re-exports all public types for working with the Habitica API.
--
-- = Overview
--
-- The Habitica API is exposed through a type-safe GADT where each operation
-- specifies its exact payload and return type:
--
-- @
-- -- These are type-safe:
-- GetUser          -- Returns UserInfo
-- GetTasks Dailys  -- Returns [HabiticaTask]
-- CreateTodo title -- Returns TodoId
-- @
--
-- = Module Structure
--
-- * "Tidepool.Habitica.Types" - Domain types (TaskId, Direction, TaskType)
-- * "Tidepool.Habitica.Response" - Response types with custom FromJSON
-- * "Tidepool.Habitica.Op" - HabiticaOp GADT
-- * "Tidepool.Habitica.Error" - HabiticaError ADT
module Tidepool.Habitica
  ( -- * Domain Types
    TaskType(..)
  , Direction(..)
  , TaskId(..)
  , TodoId(..)
  , ChecklistItemId(..)

    -- * Response Types
  , UserInfo(..)
  , UserStats(..)
  , HabiticaTask(..)
  , FetchedTodo(..)
  , FetchedChecklistItem(..)
  , ScoreResult(..)

    -- * Operations GADT
  , HabiticaOp(..)

    -- * Error Types
  , HabiticaError(..)
  ) where

import Tidepool.Habitica.Types
import Tidepool.Habitica.Response
import Tidepool.Habitica.Op
import Tidepool.Habitica.Error
