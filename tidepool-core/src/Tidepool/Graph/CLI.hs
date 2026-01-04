{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typeclass for graphs that can be run via CLI.
--
-- This module defines 'GraphCLI', enabling graphs to be wrapped as CLI tools
-- using optparse-applicative. The entry type determines CLI arguments, and
-- the exit type determines CLI output format.
--
-- = Usage
--
-- Define instances for your graphs:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry :: mode :- Entry Input
--   , ...
--   , exit :: mode :- Exit Output
--   }
--   deriving Generic
--
-- instance GraphCLI MyGraph where
--   type CLIInput MyGraph = Input
--   type CLIOutput MyGraph = Output
--
--   cliParser = Input
--     \<$\> strOption (long "name" <> help "User name")
--     \<*\> option auto (long "count" <> help "Item count")
--
--   formatOutput = T.pack . show
--
--   cliDescription = "Process items for a user"
-- @
--
-- = Generic Derivation (Future)
--
-- The companion module 'Tidepool.Graph.CLI.Generic' (gt-hsg4.2) will provide
-- automatic parser derivation from record types:
--
-- @
-- instance GraphCLI MyGraph where
--   type CLIInput MyGraph = Input
--   type CLIOutput MyGraph = Output
--
--   cliParser = genericParser  -- Derives from Input's Generic instance
--   formatOutput = genericFormat  -- Derives from Output's Show/ToJSON
--   cliDescription = "My graph CLI"
-- @
module Tidepool.Graph.CLI
  ( -- * GraphCLI Typeclass
    GraphCLI(..)

    -- * Type Families for Entry/Exit Extraction
  , GraphEntryType
  , GraphExitType
  , ExtractEntry
  , ExtractExit
  ) where

import Data.Kind (Type, Constraint)
import Data.Text (Text)
import GHC.Generics (Generic(..), M1(..), K1(..), (:*:))
import GHC.Generics qualified as G
import Options.Applicative (Parser)

import Tidepool.Graph.Generic.Core (AsGraph, Entry, Exit)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPHCLI TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for graphs that can be run via CLI.
--
-- Instances define how to parse CLI arguments into the graph's input type,
-- and how to format the graph's output for CLI display.
--
-- The associated types 'CLIInput' and 'CLIOutput' should typically match
-- the graph's Entry and Exit types. Use 'GraphEntryType' and 'GraphExitType'
-- type families to derive these automatically:
--
-- @
-- instance GraphCLI MyGraph where
--   type CLIInput MyGraph = GraphEntryType MyGraph  -- or just: Input
--   type CLIOutput MyGraph = GraphExitType MyGraph  -- or just: Output
--   ...
-- @
type GraphCLI :: (Type -> Type) -> Constraint
class GraphCLI graph where
  -- | The input type parsed from CLI arguments.
  --
  -- Typically matches the graph's Entry type:
  --
  -- @
  -- type CLIInput MyGraph = Input  -- from: entry :: mode :- Entry Input
  -- @
  type CLIInput graph :: Type

  -- | The output type formatted for CLI display.
  --
  -- Typically matches the graph's Exit type:
  --
  -- @
  -- type CLIOutput MyGraph = Output  -- from: exit :: mode :- Exit Output
  -- @
  type CLIOutput graph :: Type

  -- | Parser for CLI input using optparse-applicative.
  --
  -- Example:
  --
  -- @
  -- cliParser = MyInput
  --   \<$\> strOption (long "file" <> short 'f' <> help "Input file")
  --   \<*\> switch (long "verbose" <> short 'v' <> help "Verbose output")
  -- @
  cliParser :: Parser (CLIInput graph)

  -- | Format graph output as text for CLI display.
  --
  -- Simple approach:
  --
  -- @
  -- formatOutput = T.pack . show
  -- @
  --
  -- Or pretty-print JSON:
  --
  -- @
  -- formatOutput = TL.toStrict . Aeson.encodeToLazyText
  -- @
  formatOutput :: CLIOutput graph -> Text

  -- | Description shown in @--help@ output.
  --
  -- Default: @"Run graph"@
  cliDescription :: Text
  cliDescription = "Run graph"

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE FAMILIES FOR ENTRY/EXIT EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Entry type from a graph record's Generic representation.
--
-- @
-- GraphEntryType MyGraph = Input  -- if entry :: mode :- Entry Input
-- @
type GraphEntryType :: (Type -> Type) -> Type
type family GraphEntryType graph where
  GraphEntryType graph = ExtractEntry (Rep (graph AsGraph))

-- | Extract Exit type from a graph record's Generic representation.
--
-- @
-- GraphExitType MyGraph = Output  -- if exit :: mode :- Exit Output
-- @
type GraphExitType :: (Type -> Type) -> Type
type family GraphExitType graph where
  GraphExitType graph = ExtractExit (Rep (graph AsGraph))

-- | Extract Entry type from Generic representation.
--
-- Traverses the representation looking for @Entry a@ field types.
type ExtractEntry :: (Type -> Type) -> Type
type family ExtractEntry f where
  ExtractEntry (M1 G.D _ f) = ExtractEntry f
  ExtractEntry (M1 G.C _ f) = ExtractEntry f
  ExtractEntry (M1 G.S _ (K1 _ (Entry a))) = a
  ExtractEntry (M1 G.S _ _) = EntryNotFound
  ExtractEntry (l :*: r) = ChooseEntry (ExtractEntry l) (ExtractEntry r)
  ExtractEntry _ = EntryNotFound

-- | Extract Exit type from Generic representation.
type ExtractExit :: (Type -> Type) -> Type
type family ExtractExit f where
  ExtractExit (M1 G.D _ f) = ExtractExit f
  ExtractExit (M1 G.C _ f) = ExtractExit f
  ExtractExit (M1 G.S _ (K1 _ (Exit a))) = a
  ExtractExit (M1 G.S _ _) = ExitNotFound
  ExtractExit (l :*: r) = ChooseExit (ExtractExit l) (ExtractExit r)
  ExtractExit _ = ExitNotFound

-- | Sentinel for missing Entry.
data EntryNotFound

-- | Sentinel for missing Exit.
data ExitNotFound

-- | Choose between Entry candidates (take first non-NotFound).
type ChooseEntry :: Type -> Type -> Type
type family ChooseEntry l r where
  ChooseEntry EntryNotFound r = r
  ChooseEntry l _ = l

-- | Choose between Exit candidates (take first non-NotFound).
type ChooseExit :: Type -> Type -> Type
type family ChooseExit l r where
  ChooseExit ExitNotFound r = r
  ChooseExit l _ = l
