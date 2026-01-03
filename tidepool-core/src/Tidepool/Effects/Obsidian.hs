-- | Obsidian/PKM integration effect
--
-- = ⚠️  STUB IMPLEMENTATION - Not Yet Functional
--
-- This module provides type signatures for Obsidian vault integration
-- but all runner functions call @error@ at runtime.
--
-- __Intended use:__
--
-- * Type checking and effect composition
-- * Template for implementing real Obsidian integration
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
-- runObsidianVault :: FilePath -> Eff (Obsidian : es) a -> Eff es a
-- runObsidianVault vaultPath = interpret $ \\case
--   ListPages -> do
--     -- List .md files in vault
--     ...
--   ReadPage path -> do
--     -- Read file contents
--     ...
--   AppendToPage path content -> do
--     -- Append to file
--     ...
--   CreatePage path content -> do
--     -- Create new .md file
--     ...
-- @
module Tidepool.Effects.Obsidian
  ( -- * Effect
    Obsidian(..)
  , listPages
  , readPage
  , appendToPage
  , createPage

    -- * Types
  , PagePath(..)

    -- * Runner (stub)
  , runObsidianStub
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Effect (Log, logInfo)

-- Types

newtype PagePath = PagePath { unPagePath :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- Effect

data Obsidian r where
  ListPages    :: Obsidian [PagePath]
  ReadPage     :: PagePath -> Obsidian Text
  AppendToPage :: PagePath -> Text -> Obsidian ()
  CreatePage   :: PagePath -> Text -> Obsidian ()

listPages :: Member Obsidian effs => Eff effs [PagePath]
listPages = send ListPages

readPage :: Member Obsidian effs => PagePath -> Eff effs Text
readPage path = send (ReadPage path)

appendToPage :: Member Obsidian effs => PagePath -> Text -> Eff effs ()
appendToPage path content = send (AppendToPage path content)

createPage :: Member Obsidian effs => PagePath -> Text -> Eff effs ()
createPage path content = send (CreatePage path content)

-- Stub runner (errors on call)

runObsidianStub :: Member Log effs => Eff (Obsidian ': effs) a -> Eff effs a
runObsidianStub = interpret $ \case
  ListPages -> do
    logInfo "[Obsidian:stub] ListPages called"
    error "Obsidian.listPages: not implemented"
  ReadPage (PagePath path) -> do
    logInfo $ "[Obsidian:stub] ReadPage called: " <> path
    error "Obsidian.readPage: not implemented"
  AppendToPage (PagePath path) _ -> do
    logInfo $ "[Obsidian:stub] AppendToPage called: " <> path
    error "Obsidian.appendToPage: not implemented"
  CreatePage (PagePath path) _ -> do
    logInfo $ "[Obsidian:stub] CreatePage called: " <> path
    error "Obsidian.createPage: not implemented"
