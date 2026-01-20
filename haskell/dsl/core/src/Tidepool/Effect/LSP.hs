{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | IO-blind Language Server Protocol effect (Native-only)
--
-- = Overview
--
-- This module provides an IO-blind LSP effect for native builds.
-- LSP requires a local language server subprocess (e.g., HLS) which
-- is not available in WASM/browser environments.
--
-- All operations have a 'NativeOnly' constraint that produces
-- a helpful compile-time error if used in WASM builds.
--
-- = Architecture
--
-- @
--  +-------------------+
--  | Tidepool.Effect.LSP | <- This module (types, effect, utilities)
--  +-------------------+
--           |
--   +-------v-------+
--   | lsp-interpreter  | <- Native interpreter (via lsp-client)
--   | (lsp-client)  |
--   +---------------+
-- @
--
-- Includes:
--   - Effect definition (LSP GADT)
--   - Smart constructors for LSP queries
--   - Type definitions (Position, Diagnostic, etc.)
--
-- = Usage
--
-- Effectful handler:
--
-- @
-- myHandler :: (Member LSP effs, NativeOnly) => Eff effs Text
-- myHandler = do
--   info <- hover (textDocument "src/Main.hs") (position 42 10)
--   case info of
--     Just h -> pure (hoverContents h)
--     Nothing -> pure "No hover info"
-- @
--
module Tidepool.Effect.LSP
  ( -- * Platform Constraint
    NativeOnly

    -- * Effect
  , LSP(..)

    -- * Smart Constructors
  , diagnostics
  , hover
  , references
  , definition
  , codeActions
  , rename
  , completion
  , workspaceSymbol
  , documentSymbol
  , prepareCallHierarchy
  , outgoingCalls
  , getIndexingState

    -- * Document Identifiers
  , TextDocumentIdentifier(..)
  , textDocument

    -- * Position Types
  , Position(..)
  , position
  , Range(..)
  , range
  , Location(..)

    -- * Result Types
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , HoverInfo(..)
  , CodeAction(..)
  , CodeActionKind(..)
  , WorkspaceEdit(..)
  , TextEdit(..)
  , CompletionItem(..)
  , CompletionItemKind(..)
  , SymbolInformation(..)
  , SymbolKind(..)
  , CallHierarchyItem(..)
  , CallHierarchyOutgoingCall(..)

    -- * Indexing State
  , IndexingState(..)
  , IndexingInfo(..)
  , indexingDuration
  , getIndexingInfo
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Platform (NativeOnly)


-- ════════════════════════════════════════════════════════════════════════════
-- DOCUMENT IDENTIFIERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Identifies a text document by its URI.
--
-- In LSP, URIs typically use the file:// scheme for local files.
newtype TextDocumentIdentifier = TextDocumentIdentifier
  { tdiUri :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for a text document identifier.
--
-- @
-- textDocument "src/Main.hs"
-- -- Creates TextDocumentIdentifier with uri "file://src/Main.hs"
-- @
textDocument :: Text -> TextDocumentIdentifier
textDocument uri = TextDocumentIdentifier
  { tdiUri = if "file://" `T.isPrefixOf` uri then uri else "file://" <> uri
  }


-- ════════════════════════════════════════════════════════════════════════════
-- POSITION TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | A position in a text document (0-indexed line and character).
data Position = Position
  { posLine      :: !Int  -- ^ Line number (0-indexed)
  , posCharacter :: !Int  -- ^ Character offset in line (0-indexed)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for a position.
--
-- Note: LSP uses 0-indexed positions, but many tools show 1-indexed.
-- This constructor takes 0-indexed values.
position :: Int -> Int -> Position
position line char = Position { posLine = line, posCharacter = char }

-- | A range in a text document.
data Range = Range
  { rangeStart :: !Position  -- ^ Start position (inclusive)
  , rangeEnd   :: !Position  -- ^ End position (exclusive)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for a range.
range :: Position -> Position -> Range
range start end = Range { rangeStart = start, rangeEnd = end }

-- | A location in a document.
data Location = Location
  { locUri   :: !Text   -- ^ Document URI
  , locRange :: !Range  -- ^ Range within the document
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Diagnostic severity levels.
data DiagnosticSeverity
  = SeverityError
  | SeverityWarning
  | SeverityInformation
  | SeverityHint
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A diagnostic (error, warning, etc.) from the language server.
data Diagnostic = Diagnostic
  { diagRange    :: !Range              -- ^ Where the diagnostic applies
  , diagSeverity :: !DiagnosticSeverity -- ^ Severity level
  , diagCode     :: !(Maybe Text)       -- ^ Optional error code
  , diagSource   :: !(Maybe Text)       -- ^ Source of the diagnostic (e.g., "ghc")
  , diagMessage  :: !Text               -- ^ The diagnostic message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Hover information returned by the language server.
data HoverInfo = HoverInfo
  { hoverContents :: !Text          -- ^ The hover text (may be markdown)
  , hoverRange    :: !(Maybe Range) -- ^ Range the hover applies to
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Code action kinds.
data CodeActionKind
  = QuickFix
  | Refactor
  | RefactorExtract
  | RefactorInline
  | RefactorRewrite
  | Source
  | SourceOrganizeImports
  | SourceFixAll
  | OtherKind !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A code action (quick fix, refactoring, etc.).
data CodeAction = CodeAction
  { caTitle   :: !Text                    -- ^ Short description
  , caKind    :: !(Maybe CodeActionKind)  -- ^ Kind of code action
  , caEdit    :: !(Maybe WorkspaceEdit)   -- ^ Changes to apply
  , caCommand :: !(Maybe Text)            -- ^ Optional command to run
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A text edit.
data TextEdit = TextEdit
  { teRange   :: !Range  -- ^ Range to replace
  , teNewText :: !Text   -- ^ New text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Workspace-wide edit.
data WorkspaceEdit = WorkspaceEdit
  { weChanges :: !(Map Text [TextEdit])  -- ^ URI -> edits
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Completion item kinds.
data CompletionItemKind
  = CIKText
  | CIKMethod
  | CIKFunction
  | CIKConstructor
  | CIKField
  | CIKVariable
  | CIKClass
  | CIKInterface
  | CIKModule
  | CIKProperty
  | CIKUnit
  | CIKValue
  | CIKEnum
  | CIKKeyword
  | CIKSnippet
  | CIKColor
  | CIKFile
  | CIKReference
  | CIKFolder
  | CIKEnumMember
  | CIKConstant
  | CIKStruct
  | CIKEvent
  | CIKOperator
  | CIKTypeParameter
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A completion suggestion.
data CompletionItem = CompletionItem
  { ciLabel         :: !Text                      -- ^ Display text
  , ciKind          :: !(Maybe CompletionItemKind) -- ^ Kind of completion
  , ciDetail        :: !(Maybe Text)              -- ^ Additional details (e.g., type)
  , ciDocumentation :: !(Maybe Text)              -- ^ Documentation
  , ciInsertText    :: !(Maybe Text)              -- ^ Text to insert (if different from label)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Symbol kinds returned by workspace/symbol.
data SymbolKind
  = SKFile
  | SKModule
  | SKNamespace
  | SKPackage
  | SKClass
  | SKMethod
  | SKProperty
  | SKField
  | SKConstructor
  | SKEnum
  | SKInterface
  | SKFunction
  | SKVariable
  | SKConstant
  | SKString
  | SKNumber
  | SKBoolean
  | SKArray
  | SKObject
  | SKKey
  | SKNull
  | SKEnumMember
  | SKStruct
  | SKEvent
  | SKOperator
  | SKTypeParameter
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Information about a symbol found in workspace search.
data SymbolInformation = SymbolInformation
  { siName     :: !Text              -- ^ Symbol name
  , siKind     :: !SymbolKind        -- ^ Kind of symbol
  , siLocation :: !Location          -- ^ Where the symbol is defined
  , siContainer :: !(Maybe Text)     -- ^ Container name (e.g., module)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents a symbol in the call hierarchy.
data CallHierarchyItem = CallHierarchyItem
  { chiName           :: !Text
  , chiKind           :: !SymbolKind
  , chiUri            :: !Text
  , chiRange          :: !Range
  , chiSelectionRange :: !Range
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents an outgoing call from a function.
data CallHierarchyOutgoingCall = CallHierarchyOutgoingCall
  { chocTo         :: !CallHierarchyItem
  , chocFromRanges :: ![Range]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- INDEXING STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | HLS indexing state.
--
-- When HLS is still indexing the workspace, LSP queries may return
-- incomplete results. This state is tracked by the session and can be
-- queried by tools to add warnings to their output.
data IndexingState
  = Startup     -- ^ HLS has started but hasn't begun indexing (initial state)
  | Indexing    -- ^ HLS is still indexing the workspace
  | Ready       -- ^ HLS indexing complete, queries should be accurate
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Rich indexing information for observability.
--
-- Extends 'IndexingState' with diagnostic data useful for debugging
-- and test runs:
--
-- * Progress count: Number of incomplete HLS indexing sessions
-- * Progress tokens: Token IDs for identifying specific indexing operations
-- * Session timing: When the LSP session started and when indexing completed
--
-- Example during indexing:
--
-- @
-- IndexingInfo
--   { iiState = Indexing
--   , iiProgressCount = 3
--   , iiProgressTokens = ["hls/indexing/1", "hls/typecheck/2"]
--   , iiSessionStart = 2026-01-19 12:00:00 UTC
--   , iiReadyAt = Nothing
--   }
-- @
--
-- Example after indexing complete:
--
-- @
-- IndexingInfo
--   { iiState = Ready
--   , iiProgressCount = 0
--   , iiProgressTokens = []
--   , iiSessionStart = 2026-01-19 12:00:00 UTC
--   , iiReadyAt = Just (2026-01-19 12:00:45 UTC)
--   }
--
-- indexingDuration info = Just 45s
-- @
data IndexingInfo = IndexingInfo
  { iiState          :: !IndexingState     -- ^ Binary state (Indexing | Ready)
  , iiProgressCount  :: !Int               -- ^ Number of incomplete progress sessions
  , iiProgressTokens :: ![Text]            -- ^ Progress token IDs for debugging
  , iiSessionStart   :: !UTCTime           -- ^ When LSP session started
  , iiReadyAt        :: !(Maybe UTCTime)   -- ^ When indexing completed (if Ready)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Compute the time HLS spent indexing.
--
-- Returns 'Nothing' if indexing hasn't completed yet.
--
-- @
-- info <- getIndexingInfo
-- case indexingDuration info of
--   Just duration -> log $ "Indexing took: " <> show duration
--   Nothing -> log "Still indexing..."
-- @
indexingDuration :: IndexingInfo -> Maybe NominalDiffTime
indexingDuration info = diffUTCTime <$> info.iiReadyAt <*> pure info.iiSessionStart


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | The LSP effect provides language server capabilities.
--
-- All operations take a 'TextDocumentIdentifier' to specify the file,
-- and 'Position' or 'Range' to specify the location within the file.
--
-- This effect is IO-blind - interpreters handle the actual communication:
--
-- * Native: Use 'Tidepool.LSP.Interpreter.runLSP' with lsp-client
-- * WASM: Not available – 'NativeOnly' constraint prevents use in WASM builds
data LSP r where
  -- | Get diagnostics (errors, warnings) for a document.
  Diagnostics :: TextDocumentIdentifier -> LSP [Diagnostic]

  -- | Get hover information at a position.
  Hover :: TextDocumentIdentifier -> Position -> LSP (Maybe HoverInfo)

  -- | Find all references to the symbol at a position.
  References :: TextDocumentIdentifier -> Position -> LSP [Location]

  -- | Go to definition of the symbol at a position.
  Definition :: TextDocumentIdentifier -> Position -> LSP [Location]

  -- | Get available code actions for a range.
  CodeActions :: TextDocumentIdentifier -> Range -> LSP [CodeAction]

  -- | Rename the symbol at a position.
  Rename :: TextDocumentIdentifier -> Position -> Text -> LSP WorkspaceEdit

  -- | Get completion suggestions at a position.
  Completion :: TextDocumentIdentifier -> Position -> LSP [CompletionItem]

  -- | Search for symbols in the workspace by name query.
  WorkspaceSymbol :: Text -> LSP [SymbolInformation]

  -- | Get all symbols (functions, types, etc.) defined in a document.
  DocumentSymbol :: TextDocumentIdentifier -> LSP [SymbolInformation]

  -- | Prepare call hierarchy at a position.
  PrepareCallHierarchy :: TextDocumentIdentifier -> Position -> LSP [CallHierarchyItem]

  -- | Get outgoing calls for a call hierarchy item.
  OutgoingCalls :: CallHierarchyItem -> LSP [CallHierarchyOutgoingCall]

  -- | Get the current HLS indexing state.
  --
  -- Returns 'Indexing' if HLS is still indexing the workspace,
  -- 'Ready' when indexing is complete and queries should be accurate.
  GetIndexingState :: LSP IndexingState

  -- | Get detailed HLS indexing information.
  --
  -- Returns 'IndexingInfo' with full diagnostic data:
  -- progress count, token IDs, session timing, etc.
  GetIndexingInfo :: LSP IndexingInfo


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get diagnostics for a document.
--
-- Note: This operation requires native execution (not available in WASM).
diagnostics :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Eff effs [Diagnostic]
diagnostics doc = send (Diagnostics doc)

-- | Get hover information at a position.
--
-- Note: This operation requires native execution (not available in WASM).
hover :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Eff effs (Maybe HoverInfo)
hover doc pos = send (Hover doc pos)

-- | Find all references to a symbol.
--
-- Note: This operation requires native execution (not available in WASM).
references :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Eff effs [Location]
references doc pos = send (References doc pos)

-- | Go to definition of a symbol.
--
-- Note: This operation requires native execution (not available in WASM).
definition :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Eff effs [Location]
definition doc pos = send (Definition doc pos)

-- | Get available code actions.
--
-- Note: This operation requires native execution (not available in WASM).
codeActions :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Range -> Eff effs [CodeAction]
codeActions doc rng = send (CodeActions doc rng)

-- | Rename a symbol.
--
-- Note: This operation requires native execution (not available in WASM).
rename :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Text -> Eff effs WorkspaceEdit
rename doc pos newName = send (Rename doc pos newName)

-- | Get completion suggestions.
--
-- Note: This operation requires native execution (not available in WASM).
completion :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Eff effs [CompletionItem]
completion doc pos = send (Completion doc pos)

-- | Search for symbols in the workspace.
--
-- Note: This operation requires native execution (not available in WASM).
workspaceSymbol :: (Member LSP effs, NativeOnly) => Text -> Eff effs [SymbolInformation]
workspaceSymbol query = send (WorkspaceSymbol query)

-- | Get all symbols defined in a document.
--
-- Returns all functions, types, classes, etc. defined in the file with their ranges.
--
-- Note: This operation requires native execution (not available in WASM).
documentSymbol :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Eff effs [SymbolInformation]
documentSymbol doc = send (DocumentSymbol doc)

-- | Prepare call hierarchy at a position.
--
-- Note: This operation requires native execution (not available in WASM).
prepareCallHierarchy :: (Member LSP effs, NativeOnly) => TextDocumentIdentifier -> Position -> Eff effs [CallHierarchyItem]
prepareCallHierarchy doc pos = send (PrepareCallHierarchy doc pos)

-- | Get outgoing calls for a call hierarchy item.
--
-- Note: This operation requires native execution (not available in WASM).
outgoingCalls :: (Member LSP effs, NativeOnly) => CallHierarchyItem -> Eff effs [CallHierarchyOutgoingCall]
outgoingCalls item = send (OutgoingCalls item)

-- | Get the current HLS indexing state.
--
-- Returns 'Indexing' if HLS is still indexing the workspace (results may be incomplete),
-- or 'Ready' when indexing is complete and queries should be accurate.
--
-- Use this to add warnings to tool results when HLS is still indexing:
--
-- @
-- state <- getIndexingState
-- let warning = case state of
--       Indexing -> Just "HLS is still indexing. Results may be incomplete."
--       Ready -> Nothing
-- @
--
-- Note: This operation requires native execution (not available in WASM).
getIndexingState :: (Member LSP effs, NativeOnly) => Eff effs IndexingState
getIndexingState = send GetIndexingState

-- | Get detailed HLS indexing information.
--
-- Returns 'IndexingInfo' with full diagnostic data including:
--
-- * Progress count and token IDs (for debugging HLS behavior)
-- * Session start time and ready timestamp (for measuring indexing duration)
--
-- Use 'indexingDuration' to compute how long HLS spent indexing:
--
-- @
-- info <- getIndexingInfo
-- case indexingDuration info of
--   Just duration -> log $ "HLS indexed in " <> show duration
--   Nothing -> log $ "Still indexing, " <> show (iiProgressCount info) <> " tasks remaining"
-- @
--
-- Note: This operation requires native execution (not available in WASM).
getIndexingInfo :: (Member LSP effs, NativeOnly) => Eff effs IndexingInfo
getIndexingInfo = send GetIndexingInfo
