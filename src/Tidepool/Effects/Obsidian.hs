-- | Obsidian/PKM integration effect
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
import Effectful
import Effectful.Dispatch.Dynamic

import Tidepool.Effect (Log, logInfo)

-- Types

newtype PagePath = PagePath { unPagePath :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- Effect

data Obsidian :: Effect where
  ListPages    :: Obsidian m [PagePath]
  ReadPage     :: PagePath -> Obsidian m Text
  AppendToPage :: PagePath -> Text -> Obsidian m ()
  CreatePage   :: PagePath -> Text -> Obsidian m ()

type instance DispatchOf Obsidian = 'Dynamic

listPages :: Obsidian :> es => Eff es [PagePath]
listPages = send ListPages

readPage :: Obsidian :> es => PagePath -> Eff es Text
readPage path = send (ReadPage path)

appendToPage :: Obsidian :> es => PagePath -> Text -> Eff es ()
appendToPage path content = send (AppendToPage path content)

createPage :: Obsidian :> es => PagePath -> Text -> Eff es ()
createPage path content = send (CreatePage path content)

-- Stub runner (errors on call)

runObsidianStub :: (IOE :> es, Log :> es) => Eff (Obsidian : es) a -> Eff es a
runObsidianStub = interpret $ \_ -> \case
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
