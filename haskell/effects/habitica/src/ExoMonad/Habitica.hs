{-# LANGUAGE DuplicateRecordFields #-}

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
-- * "ExoMonad.Habitica.Types" - Domain types (TaskId, Direction, TaskType)
-- * "ExoMonad.Habitica.Response" - Response types with custom FromJSON
-- * "ExoMonad.Habitica.Op" - HabiticaOp GADT
-- * "ExoMonad.Habitica.Error" - HabiticaError ADT
module ExoMonad.Habitica
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

import ExoMonad.Habitica.Types
import ExoMonad.Habitica.Response
import ExoMonad.Habitica.Op
import ExoMonad.Habitica.Error
