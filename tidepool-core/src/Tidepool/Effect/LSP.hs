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
--   | lsp-executor  | <- Native interpreter (via lsp-client)
--   | (lsp-client)  |
--   +---------------+
-- @
--
-- Includes:
--   - Effect definition (LSP GADT)
--   - Smart constructors for LSP queries
--   - Type definitions (Position, Diagnostic, etc.)
--   - Stub runner for testing (runLSPStub)
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
-- Stub runner for testing:
--
-- @
-- import Tidepool.Effect (runLog, LogLevel(..))
--
-- test :: IO ()
-- test = void $ runM $ runLog InfoLevel $ runLSPStub $ do
--   diags <- diagnostics (textDocument "src/Main.hs")
--   pure ()
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

    -- * Native-only utilities
  , runLSPStub
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effect (Log, logInfo)
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
-- * Native: Use 'Tidepool.LSP.Executor.runLSP' with lsp-client
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


-- ════════════════════════════════════════════════════════════════════════════
-- NATIVE-ONLY UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs operations and returns empty/error results.
--
-- This is a placeholder for testing and development. For real LSP
-- functionality, use 'Tidepool.LSP.Executor.runLSP' from lsp-executor.
runLSPStub :: Member Log effs => Eff (LSP ': effs) a -> Eff effs a
runLSPStub = interpret $ \case
  Diagnostics doc -> do
    logInfo $ "[LSP:stub] Diagnostics called for: " <> doc.tdiUri
    pure []  -- Return empty list instead of erroring

  Hover doc pos -> do
    logInfo $ "[LSP:stub] Hover at " <> doc.tdiUri <> " " <> showPos pos
    pure Nothing

  References doc pos -> do
    logInfo $ "[LSP:stub] References at " <> doc.tdiUri <> " " <> showPos pos
    pure []

  Definition doc pos -> do
    logInfo $ "[LSP:stub] Definition at " <> doc.tdiUri <> " " <> showPos pos
    pure []

  CodeActions doc rng -> do
    logInfo $ "[LSP:stub] CodeActions at " <> doc.tdiUri <> " " <> showRange rng
    pure []

  Rename doc pos newName -> do
    logInfo $ "[LSP:stub] Rename at " <> doc.tdiUri <> " " <> showPos pos <> " to " <> newName
    pure (WorkspaceEdit Map.empty)

  Completion doc pos -> do
    logInfo $ "[LSP:stub] Completion at " <> doc.tdiUri <> " " <> showPos pos
    pure []
  where
    showPos :: Position -> Text
    showPos p = "(" <> showT p.posLine <> ":" <> showT p.posCharacter <> ")"

    showRange :: Range -> Text
    showRange r = showPos r.rangeStart <> "-" <> showPos r.rangeEnd

    showT :: Show a => a -> Text
    showT = T.pack . show
