-- | LSP effect interpreter using lsp-client.
--
-- Interprets 'LSP' effects by communicating with a language server
-- via the lsp-client library.
--
module Tidepool.LSP.Interpreter
  ( -- * Session Management
    LSPSession
  , withLSPSession

    -- * Effect Interpreter
  , runLSP
  ) where

import Control.Exception (bracket)
import Control.Monad.Freer (Eff, LastMember, sendM, interpret)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.IO (Handle)
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle, createProcess, proc, terminateProcess, cwd)

import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize, request)
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L

import Tidepool.Effect.LSP


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION MANAGEMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | An active LSP session with a language server.
data LSPSession = LSPSession
  { lspStdin   :: !Handle
  , lspStdout  :: !Handle
  , lspProcess :: !ProcessHandle
  }

-- | Start an LSP session with HLS and run an action.
--
-- @
-- withLSPSession "/path/to/project" $ \session -> do
--   result <- runLSP session myLSPAction
--   pure result
-- @
withLSPSession
  :: FilePath              -- ^ Project root directory
  -> (LSPSession -> IO a)  -- ^ Action to run with session
  -> IO a
withLSPSession rootDir action =
  bracket acquire release $ \session ->
    action session
  where
    acquire = do
      (Just stdin, Just stdout, _, ph) <- createProcess
        (proc "haskell-language-server-wrapper" ["--lsp"])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , cwd = Just rootDir
          }
      pure $ LSPSession stdin stdout ph

    release session =
      terminateProcess session.lspProcess


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LSP effects using an active session.
runLSP :: LastMember IO effs => LSPSession -> Eff (LSP ': effs) a -> Eff effs a
runLSP session = interpret $ \case
  Diagnostics _doc ->
    -- Diagnostics come via notifications, not requests
    -- Would need to track them in session state
    pure []

  Hover doc pos -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentHover $ L.HoverParams
      { L._textDocument = toTextDocumentId doc
      , L._position = toPosition pos
      , L._workDoneToken = Nothing
      }
    pure $ case resp._result of
      Right result -> fromHover result
      Left _err -> Nothing

  References doc pos -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentReferences $ L.ReferenceParams
      { L._textDocument = toTextDocumentId doc
      , L._position = toPosition pos
      , L._workDoneToken = Nothing
      , L._partialResultToken = Nothing
      , L._context = L.ReferenceContext { L._includeDeclaration = True }
      }
    pure $ case resp._result of
      Right result -> fromLocations result
      Left _err -> []

  Definition doc pos -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentDefinition $ L.DefinitionParams
      { L._textDocument = toTextDocumentId doc
      , L._position = toPosition pos
      , L._workDoneToken = Nothing
      , L._partialResultToken = Nothing
      }
    pure $ case resp._result of
      Right result -> fromDefinition result
      Left _err -> []

  CodeActions doc rng -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentCodeAction $ L.CodeActionParams
      { L._textDocument = toTextDocumentId doc
      , L._range = toRange rng
      , L._context = L.CodeActionContext
          { L._diagnostics = []
          , L._only = Nothing
          , L._triggerKind = Nothing
          }
      , L._workDoneToken = Nothing
      , L._partialResultToken = Nothing
      }
    pure $ case resp._result of
      Right result -> fromCodeActions result
      Left _err -> []

  Rename doc pos newName -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentRename $ L.RenameParams
      { L._textDocument = toTextDocumentId doc
      , L._position = toPosition pos
      , L._newName = newName
      , L._workDoneToken = Nothing
      }
    pure $ case resp._result of
      Right result -> fromWorkspaceEdit result
      Left _err -> WorkspaceEdit Map.empty

  Completion doc pos -> sendM $ runSession session $ do
    resp <- request L.SMethod_TextDocumentCompletion $ L.CompletionParams
      { L._textDocument = toTextDocumentId doc
      , L._position = toPosition pos
      , L._workDoneToken = Nothing
      , L._partialResultToken = Nothing
      , L._context = Nothing
      }
    pure $ case resp._result of
      Right result -> fromCompletions result
      Left _err -> []

-- | Run a Session action within an LSPSession.
-- runSessionWithHandles takes: (serverOutput, serverInput) i.e. (read, write)
runSession :: LSPSession -> Session a -> IO a
runSession session action =
  runSessionWithHandles session.lspStdout session.lspStdin $ do
    _ <- initialize Nothing
    action


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE CONVERSIONS: Our types -> lsp-types
-- ════════════════════════════════════════════════════════════════════════════

toTextDocumentId :: TextDocumentIdentifier -> L.TextDocumentIdentifier
toTextDocumentId doc = L.TextDocumentIdentifier
  { L._uri = L.Uri doc.tdiUri
  }

toPosition :: Position -> L.Position
toPosition pos = L.Position
  { L._line = fromIntegral pos.posLine
  , L._character = fromIntegral pos.posCharacter
  }

toRange :: Range -> L.Range
toRange rng = L.Range
  { L._start = toPosition rng.rangeStart
  , L._end = toPosition rng.rangeEnd
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE CONVERSIONS: lsp-types -> Our types
-- ════════════════════════════════════════════════════════════════════════════

fromPosition :: L.Position -> Position
fromPosition pos = Position
  { posLine = fromIntegral pos._line
  , posCharacter = fromIntegral pos._character
  }

fromRange :: L.Range -> Range
fromRange rng = Range
  { rangeStart = fromPosition rng._start
  , rangeEnd = fromPosition rng._end
  }

fromLocation :: L.Location -> Location
fromLocation loc = Location
  { locUri = let L.Uri u = loc._uri in u
  , locRange = fromRange loc._range
  }

-- Result type is Hover |? Null (not Maybe Hover)
fromHover :: L.Hover L.|? L.Null -> Maybe HoverInfo
fromHover (L.InR L.Null) = Nothing
fromHover (L.InL h) = Just HoverInfo
  { hoverContents = extractMarkupContent h._contents
  , hoverRange = fromRange <$> h._range
  }

extractMarkupContent :: L.MarkupContent L.|? (L.MarkedString L.|? [L.MarkedString]) -> Text
extractMarkupContent content = case content of
  L.InL mc -> mc._value  -- MarkupContent has _value field
  L.InR msOrList -> case msOrList of
    L.InL ms -> markedStringToText ms
    L.InR msList -> T.intercalate "\n" (map markedStringToText msList)

-- MarkedString is a newtype: newtype MarkedString = MarkedString (Text |? MarkedStringWithLanguage)
markedStringToText :: L.MarkedString -> Text
markedStringToText (L.MarkedString inner) = case inner of
  L.InL t -> t
  L.InR mswl -> mswl._value

-- Result type is [Location] |? Null
fromLocations :: [L.Location] L.|? L.Null -> [Location]
fromLocations (L.InR L.Null) = []
fromLocations (L.InL locs) = map fromLocation locs

-- Result type is Definition |? ([DefinitionLink] |? Null)
fromDefinition :: L.Definition L.|? ([L.DefinitionLink] L.|? L.Null) -> [Location]
fromDefinition defOrLinksOrNull = case defOrLinksOrNull of
  L.InL (L.Definition locOrLocs) -> case locOrLocs of
    L.InL loc -> [fromLocation loc]
    L.InR locs -> map fromLocation locs
  L.InR linksOrNull -> case linksOrNull of
    L.InL links -> map fromDefinitionLink links
    L.InR L.Null -> []

fromDefinitionLink :: L.DefinitionLink -> Location
fromDefinitionLink (L.DefinitionLink link) = Location
  { locUri = let L.Uri u = link._targetUri in u
  , locRange = fromRange link._targetRange
  }

-- Result type is [Command |? CodeAction] |? Null
fromCodeActions :: [L.Command L.|? L.CodeAction] L.|? L.Null -> [CodeAction]
fromCodeActions (L.InR L.Null) = []
fromCodeActions (L.InL items) = concatMap fromCommandOrAction items

fromCommandOrAction :: L.Command L.|? L.CodeAction -> [CodeAction]
fromCommandOrAction cmdOrAction = case cmdOrAction of
  L.InL _cmd -> []  -- Skip commands, only return code actions
  L.InR ca -> [fromCodeAction ca]

fromCodeAction :: L.CodeAction -> CodeAction
fromCodeAction ca = CodeAction
  { caTitle = ca._title
  , caKind = fromCodeActionKind <$> ca._kind
  , caEdit = fromWorkspaceEditMaybe ca._edit
  , caCommand = fmap (._title) ca._command
  }

fromCodeActionKind :: L.CodeActionKind -> CodeActionKind
fromCodeActionKind k = case k of
  L.CodeActionKind_QuickFix -> QuickFix
  L.CodeActionKind_Refactor -> Refactor
  L.CodeActionKind_RefactorExtract -> RefactorExtract
  L.CodeActionKind_RefactorInline -> RefactorInline
  L.CodeActionKind_RefactorRewrite -> RefactorRewrite
  L.CodeActionKind_Source -> Source
  L.CodeActionKind_SourceOrganizeImports -> SourceOrganizeImports
  L.CodeActionKind_SourceFixAll -> SourceFixAll
  L.CodeActionKind_Custom t -> OtherKind t
  -- lsp-types may add new constructors; map unknown ones to OtherKind
  L.CodeActionKind_Empty -> OtherKind ""

-- Result type is WorkspaceEdit |? Null
fromWorkspaceEdit :: L.WorkspaceEdit L.|? L.Null -> WorkspaceEdit
fromWorkspaceEdit (L.InR L.Null) = WorkspaceEdit Map.empty
fromWorkspaceEdit (L.InL we) = fromWorkspaceEditMaybe (Just we)
  & maybe (WorkspaceEdit Map.empty) id

fromWorkspaceEditMaybe :: Maybe L.WorkspaceEdit -> Maybe WorkspaceEdit
fromWorkspaceEditMaybe Nothing = Nothing
fromWorkspaceEditMaybe (Just we) = Just $ WorkspaceEdit $
  case we._changes of
    Nothing -> Map.empty
    Just changes -> Map.fromList
      [ (uri, map fromTextEdit edits)
      | (L.Uri uri, edits) <- Map.toList changes
      ]

fromTextEdit :: L.TextEdit -> TextEdit
fromTextEdit te = TextEdit
  { teRange = fromRange te._range
  , teNewText = te._newText
  }

-- Result type is [CompletionItem] |? (CompletionList |? Null)
fromCompletions :: [L.CompletionItem] L.|? (L.CompletionList L.|? L.Null) -> [CompletionItem]
fromCompletions result = case result of
  L.InL items -> map fromCompletionItem items
  L.InR listOrNull -> case listOrNull of
    L.InL cl -> map fromCompletionItem cl._items
    L.InR L.Null -> []

fromCompletionItem :: L.CompletionItem -> CompletionItem
fromCompletionItem ci = CompletionItem
  { ciLabel = ci._label
  , ciKind = fromCompletionItemKind <$> ci._kind
  , ciDetail = ci._detail
  , ciDocumentation = extractDocumentation <$> ci._documentation
  , ciInsertText = ci._insertText
  }

-- CompletionItem._documentation is Text |? MarkupContent (in that order)
extractDocumentation :: Text L.|? L.MarkupContent -> Text
extractDocumentation doc = case doc of
  L.InL t -> t
  L.InR mc -> mc._value

fromCompletionItemKind :: L.CompletionItemKind -> CompletionItemKind
fromCompletionItemKind k = case k of
  L.CompletionItemKind_Text -> CIKText
  L.CompletionItemKind_Method -> CIKMethod
  L.CompletionItemKind_Function -> CIKFunction
  L.CompletionItemKind_Constructor -> CIKConstructor
  L.CompletionItemKind_Field -> CIKField
  L.CompletionItemKind_Variable -> CIKVariable
  L.CompletionItemKind_Class -> CIKClass
  L.CompletionItemKind_Interface -> CIKInterface
  L.CompletionItemKind_Module -> CIKModule
  L.CompletionItemKind_Property -> CIKProperty
  L.CompletionItemKind_Unit -> CIKUnit
  L.CompletionItemKind_Value -> CIKValue
  L.CompletionItemKind_Enum -> CIKEnum
  L.CompletionItemKind_Keyword -> CIKKeyword
  L.CompletionItemKind_Snippet -> CIKSnippet
  L.CompletionItemKind_Color -> CIKColor
  L.CompletionItemKind_File -> CIKFile
  L.CompletionItemKind_Reference -> CIKReference
  L.CompletionItemKind_Folder -> CIKFolder
  L.CompletionItemKind_EnumMember -> CIKEnumMember
  L.CompletionItemKind_Constant -> CIKConstant
  L.CompletionItemKind_Struct -> CIKStruct
  L.CompletionItemKind_Event -> CIKEvent
  L.CompletionItemKind_Operator -> CIKOperator
  L.CompletionItemKind_TypeParameter -> CIKTypeParameter

