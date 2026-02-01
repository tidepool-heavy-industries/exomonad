{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Exploration effects for querying codebase intelligence (LSP, ast-grep).
module ExoMonad.Guest.Effects.Explore
  ( -- * Effect type
    Explore (..),

    -- * Smart constructors
    astGrep,
    lspReferences,
    lspDefinition,
    lspHover,
    readFileRange,

    -- * Interpreter
    runExplore,

    -- * Types
    Position (..),
    Range (..),
    Location (..),
    AstGrepInput (..),
    LspQueryInput (..),
    ReadFileRangeInput (..),
    ReadFileRangeOutput (..),
    HostResult (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified
import ExoMonad.Guest.HostCall
  ( callHost,
    host_explore_ast_grep,
    host_explore_lsp_definition,
    host_explore_lsp_hover,
    host_explore_lsp_references,
    host_explore_read_file_range,
  )
import GHC.Generics (Generic)
import Prelude hiding (readFile)

-- ============================================================================
-- Types
-- ============================================================================

-- | Position in a file (0-indexed).
data Position = Position
  { posLine :: Int,
    posCharacter :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \v ->
    Position
      <$> v .: "line"
      <*> v .: "character"

instance ToJSON Position where
  toJSON (Position l c) =
    object
      [ "line" .= l,
        "character" .= c
      ]

-- | Range in a file.
data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }
  deriving (Show, Eq, Generic)

instance FromJSON Range where
  parseJSON = withObject "Range" $ \v ->
    Range
      <$> v .: "start"
      <*> v .: "end"

instance ToJSON Range where
  toJSON (Range s e) =
    object
      [ "start" .= s,
        "end" .= e
      ]

-- | Location in a file (Range + URI/Path).
data Location = Location
  { locUri :: Text,
    locRange :: Range,
    locContext :: Maybe Text -- Snippet of code at location
  }
  deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \v ->
    Location
      <$> v .: "uri"
      <*> v .: "range"
      <*> v .: "context"

instance ToJSON Location where
  toJSON (Location u r c) =
    object
      [ "uri" .= u,
        "range" .= r,
        "context" .= c
      ]

-- | Input for ast-grep search.
data AstGrepInput = AstGrepInput
  { agLanguage :: Text,
    agPattern :: Text,
    agPath :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AstGrepInput where
  toJSON (AstGrepInput l p pa) =
    object
      [ "language" .= l,
        "pattern" .= p,
        "path" .= pa
      ]

-- | Input for LSP queries (refs, def, hover).
data LspQueryInput = LspQueryInput
  { lspPath :: Text,
    lspPosition :: Position
  }
  deriving (Show, Eq, Generic)

instance ToJSON LspQueryInput where
  toJSON (LspQueryInput p pos) =
    object
      [ "path" .= p,
        "position" .= pos
      ]

-- | Input for reading file range.
data ReadFileRangeInput = ReadFileRangeInput
  { rfrPath :: Text,
    rfrStartLine :: Int,
    rfrEndLine :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReadFileRangeInput where
  toJSON (ReadFileRangeInput p s e) =
    object
      [ "path" .= p,
        "start_line" .= s,
        "end_line" .= e
      ]

-- | Output for reading file range.
data ReadFileRangeOutput = ReadFileRangeOutput
  { rfroContent :: Text,
    rfroStartLine :: Int,
    rfroEndLine :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadFileRangeOutput where
  parseJSON = withObject "ReadFileRangeOutput" $ \v ->
    ReadFileRangeOutput
      <$> v .: "content"
      <*> v .: "start_line"
      <*> v .: "end_line"

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind" :: Parser Text
    case kind of
      "Success" -> Success <$> v .: "payload"
      "Error" -> do
        errObj <- v .: "payload"
        HostError <$> (errObj .: "message")
      _ -> fail "Unknown HostResult kind"

-- ============================================================================
-- Effect type
-- ============================================================================

data Explore r where
  AstGrep :: Text -> Text -> Text -> Explore [Location]
  LspReferences :: Text -> Position -> Explore [Location]
  LspDefinition :: Text -> Position -> Explore [Location]
  LspHover :: Text -> Position -> Explore (Maybe Text)
  ReadFileRange :: Text -> Int -> Int -> Explore ReadFileRangeOutput

-- ============================================================================
-- Smart constructors
-- ============================================================================

astGrep :: (Member Explore effs) => Text -> Text -> Text -> Eff effs [Location]
astGrep lang pat path = send (AstGrep lang pat path)

lspReferences :: (Member Explore effs) => Text -> Position -> Eff effs [Location]
lspReferences path pos = send (LspReferences path pos)

lspDefinition :: (Member Explore effs) => Text -> Position -> Eff effs [Location]
lspDefinition path pos = send (LspDefinition path pos)

lspHover :: (Member Explore effs) => Text -> Position -> Eff effs (Maybe Text)
lspHover path pos = send (LspHover path pos)

readFileRange :: (Member Explore effs) => Text -> Int -> Int -> Eff effs ReadFileRangeOutput
readFileRange path start end = send (ReadFileRange path start end)

-- ============================================================================
-- Interpreter
-- ============================================================================

runExplore :: (LastMember IO effs) => Eff (Explore ': effs) a -> Eff effs a
runExplore = interpret $ \case
  AstGrep lang pat path -> sendM $ do
    let input = AstGrepInput lang pat path
    res <- callHost host_explore_ast_grep input
    pure $ case res of
      Left _ -> []
      Right (Success locations) -> locations
      Right (HostError _) -> []
  LspReferences path pos -> sendM $ do
    let input = LspQueryInput path pos
    res <- callHost host_explore_lsp_references input
    pure $ case res of
      Left _ -> []
      Right (Success locs) -> locs
      Right (HostError _) -> []
  LspDefinition path pos -> sendM $ do
    let input = LspQueryInput path pos
    res <- callHost host_explore_lsp_definition input
    pure $ case res of
      Left _ -> []
      Right (Success locs) -> locs
      Right (HostError _) -> []
  LspHover path pos -> sendM $ do
    let input = LspQueryInput path pos
    res <- callHost host_explore_lsp_hover input
    pure $ case res of
      Left _ -> Nothing
      Right (Success val) -> val
      Right (HostError _) -> Nothing
  ReadFileRange path start end -> sendM $ do
    let input = ReadFileRangeInput path start end
    res <- callHost host_explore_read_file_range input
    pure $ case res of
      Left err -> error err
      Right (Success val) -> val
      Right (HostError msg) -> error (Data.Text.unpack msg)
