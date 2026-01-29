-- | LSP effect interpreter using lsp-test.
--
-- Interprets 'LSP' effects by communicating with HLS via lsp-test.
-- lsp-test handles rootUri correctly (fixes empty workspaceFolders bug).
--
module ExoMonad.LSP.Interpreter
  ( -- * Session Management
    LSPSession
  , withLSPSession

    -- * Indexing State (re-exported from effect module)
  , IndexingState(..)
  , IndexingInfo(..)
  , getSessionIndexingState
  , getSessionIndexingInfo

    -- * Effect Interpreter
  , runLSP
  ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Exception (SomeException, throwIO, try)
import qualified Data.Set as Set
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad (when)
import Control.Monad.Freer (Eff, LastMember, sendM, interpret)
import qualified System.Process
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

-- lsp-test provides the Session monad and high-level API
import Language.LSP.Test
  ( Session, runSessionWithConfig, defaultConfig, fullLatestClientCaps
  , openDoc
  , getHover, getReferences, getDefinitions, getCompletions
  , getCodeActions, request
  , SessionConfig(..)
  , getIncompleteProgressSessions
  )
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L

import ExoMonad.Effect.LSP
  ( LSP(..), IndexingState(..), IndexingInfo(..)
  , TextDocumentIdentifier(..), Position(..), Range(..), Location(..)
  , HoverInfo(..), CodeAction(..), CodeActionKind(..), WorkspaceEdit(..)
  , TextEdit(..), CompletionItem(..), CompletionItemKind(..)
  , SymbolInformation(..), SymbolKind(..)
  , CallHierarchyItem(..), CallHierarchyOutgoingCall(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION MANAGEMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Request sent to the background LSP worker thread.
data LSPRequest = forall a. LSPRequest
  { lspReqAction :: Session a
  , lspReqResult :: MVar (Either SomeException a)
  }

-- | An active LSP session handle.
--
-- The actual Session monad runs inside withLSPSession's scope.
-- External callers communicate via the request channel.
data LSPSession = LSPSession
  { lspRequestChan  :: !(Chan LSPRequest)
  , lspIndexingInfo :: !(TVar IndexingInfo)  -- ^ Tracks HLS indexing progress with full details
  }

-- | Read the current indexing state from a session (backward compatible).
--
-- Use this to check if HLS is still indexing before making queries,
-- or to add warnings to results when indexing is incomplete.
getSessionIndexingState :: LSPSession -> IO IndexingState
getSessionIndexingState session = (.iiState) <$> readTVarIO session.lspIndexingInfo

-- | Read full indexing information from a session.
--
-- Returns 'IndexingInfo' with diagnostic details:
-- progress count, token IDs, session timing, etc.
getSessionIndexingInfo :: LSPSession -> IO IndexingInfo
getSessionIndexingInfo session = readTVarIO session.lspIndexingInfo

-- | Session configuration optimized for long-running MCP server use.
--
-- Key settings:
-- - logMessages = False: Prevents stdout bottleneck
-- - messageTimeout = 120s: Allows time for large project indexing
productionConfig :: SessionConfig
productionConfig = defaultConfig
  { messageTimeout = 120  -- 2 minutes for large projects
  -- Note: logMessages is not available in older lsp-test versions
  -- If available, set: logMessages = False
  }

-- | Start an LSP session with HLS and run an action.
--
-- lsp-test handles:
-- - Spawning HLS process
-- - rootUri configuration (FIXES the empty workspaceFolders bug)
-- - initialize/initialized handshake
-- - Capability negotiation
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
withLSPSession rootDir action = do
  -- Create request channel for communication with worker
  requestChan <- newChan
  resultMVar <- newEmptyMVar
  doneMVar <- newEmptyMVar  -- Signal when user action completes

  -- Capture session start time for indexing duration tracking
  sessionStart <- getCurrentTime

  -- Initialize with full IndexingInfo (HLS starts indexing on connect)
  let initialInfo = IndexingInfo
        { iiState = Startup
        , iiProgressCount = 0
        , iiProgressTokens = []
        , iiSessionStart = sessionStart
        , iiReadyAt = Nothing
        }
  indexingInfo <- newTVarIO initialInfo

  -- lsp-test's runSessionWithConfig spawns HLS and handles handshake
  -- The session runs until we're done or HLS crashes
  --
  -- Note: lsp-test doesn't pass --lsp flag by default, so we use a wrapper script
  -- Create wrapper if not exists (idempotent)
  let wrapperPath = "/tmp/exomonad-hls-wrapper"
  liftIO $ writeFile wrapperPath "#!/bin/bash\nexec haskell-language-server-wrapper --lsp \"$@\"\n"
  liftIO $ System.Process.callCommand $ "chmod +x " ++ wrapperPath

  runSessionWithConfig productionConfig wrapperPath fullLatestClientCaps rootDir $ do
    liftIO $ hPutStrLn stderr "[LSP] Session started, HLS initialized"

    -- Provide session handle to caller
    let session = LSPSession requestChan indexingInfo

    -- Fork user action in a separate thread so we can process requests
    liftIO $ do
      _ <- forkIO $ do
        result <- try @SomeException (action session)
        putMVar resultMVar result
        putMVar doneMVar ()  -- Signal completion
      pure ()

    -- Process requests until user action completes
    -- This loop runs inside the Session monad
    --
    -- Note: SessionConfig has ignoreLogNotifications=True (default) which
    -- prevents memory leaks from HLS's chatty log/progress notifications.
    --
    -- Indexing state is polled every 2 seconds to track HLS progress.
    --
    -- Do an immediate first poll to capture initial indexing state
    liftIO $ hPutStrLn stderr "[LSP] Performing initial indexing check..."
    initialIncomplete <- getIncompleteProgressSessions
    let initialCount = Set.size initialIncomplete
    liftIO $ hPutStrLn stderr $ "[LSP] Initial state: "
      <> show initialCount <> " incomplete progress sessions"
    when (initialCount > 0) $ do
      let tokens = map showProgressToken (Set.toList initialIncomplete)
      liftIO $ hPutStrLn stderr $ "[LSP] Progress tokens: " <> show tokens

    startTime <- liftIO getCurrentTime
    let processRequests lastCheck = do
          -- Check indexing state every 2 seconds
          now <- liftIO getCurrentTime
          nextCheck <- if diffUTCTime now lastCheck > 2
            then do
              -- Read current info to detect state transitions
              currentInfo <- liftIO $ readTVarIO indexingInfo

              -- Poll HLS for incomplete progress sessions
              incomplete <- getIncompleteProgressSessions
              let progressCount = Set.size incomplete
              let progressTokens = map showProgressToken (Set.toList incomplete)
              
              let hasProgress = progressCount > 0
              let timeSinceStart = diffUTCTime now sessionStart

              let newState = case (currentInfo.iiState, hasProgress) of
                    (_, True) -> Indexing
                    (Startup, False) -> 
                      if timeSinceStart > 15 
                        then Ready
                        else Startup
                    (Indexing, False) -> Ready
                    (Ready, False) -> Ready

              -- Set readyAt only on Indexing→Ready or Startup→Ready transition
              let readyAt = case (currentInfo.iiState, newState) of
                    (Indexing, Ready) -> Just now
                    (Startup, Ready)  -> Just now
                    _                 -> currentInfo.iiReadyAt

              -- Build updated info
              let newInfo = IndexingInfo
                    { iiState = newState
                    , iiProgressCount = progressCount
                    , iiProgressTokens = progressTokens
                    , iiSessionStart = sessionStart  -- preserved from outer scope
                    , iiReadyAt = readyAt
                    }

              -- Update the TVar with full info
              liftIO $ atomically $ writeTVar indexingInfo newInfo

              -- Log state transition (only on change)
              when (currentInfo.iiState /= newState) $
                liftIO $ hPutStrLn stderr $ "[LSP] Indexing: " <> show newState
                  <> " (count=" <> show progressCount <> ")"

              pure now
            else pure lastCheck

          -- Check if user action is done (non-blocking)
          maybeDone <- liftIO $ tryTakeMVar doneMVar
          case maybeDone of
            Just () -> pure ()  -- User action completed, exit
            Nothing -> do
              -- Check for pending request (100ms timeout)
              maybeReq <- liftIO $ timeout 100000 $ readChan requestChan
              case maybeReq of
                Nothing -> processRequests nextCheck  -- No request, loop
                Just (LSPRequest reqAction reqResponseMVar) -> do
                  liftIO $ hPutStrLn stderr "[LSP] Processing request..."
                  -- Execute the Session action
                  -- Exceptions will propagate and be handled by the caller
                  result <- reqAction
                  liftIO $ putMVar reqResponseMVar (Right result)
                  liftIO $ hPutStrLn stderr "[LSP] Request completed"
                  processRequests nextCheck

    processRequests startTime

  -- Return result from user action
  resultOrErr <- takeMVar resultMVar
  case resultOrErr of
    Right val -> pure val
    Left err -> throwIO err


-- | Default timeout for LSP requests (60 seconds).
defaultRequestTimeout :: Int
defaultRequestTimeout = 60 * 1000000  -- microseconds

-- | Execute a Session action via the session handle.
--
-- Sends the action to the worker, waits for response with timeout.
executeSession :: LSPSession -> Session a -> IO a
executeSession session action = do
  responseMVar <- newEmptyMVar
  writeChan session.lspRequestChan $ LSPRequest action responseMVar

  -- Wait for response with timeout
  maybeResult <- timeout defaultRequestTimeout $ takeMVar responseMVar

  case maybeResult of
    Nothing ->
      fail "LSP request timed out. HLS may still be indexing the project."
    Just result ->
      case result of
        Right value -> pure value
        Left err -> throwIO err


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

  Hover doc pos -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspPos = toPosition pos
    -- lsp-test's getHover takes TextDocumentIdentifier and Position
    result <- getHover lspDoc lspPos
    pure $ fromHoverResult result

  References doc pos -> sendM $ executeSession session $ do
    let lspPos = toPosition pos
        filePath = uriToFilePath doc.tdiUri
    -- Open the document first - HLS requires this for references to work
    lspDoc <- openDoc filePath "haskell"
    -- getReferences returns [Location]
    locs <- getReferences lspDoc lspPos True  -- includeDeclaration = True
    pure $ map fromLocation locs

  Definition doc pos -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspPos = toPosition pos
    -- getDefinitions returns [Location] | [LocationLink]
    result <- getDefinitions lspDoc lspPos
    pure $ fromDefinitionResult result

  CodeActions doc rng -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspRange = toRange rng
    -- getCodeActions returns [Command |? CodeAction]
    actions <- getCodeActions lspDoc lspRange
    pure $ fromCodeActionsResult actions

  Rename doc pos newName -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspPos = toPosition pos
    -- lsp-test's rename returns (), we need to use the low-level request API
    let params = L.RenameParams
          { L._workDoneToken = Nothing
          , L._textDocument = lspDoc
          , L._position = lspPos
          , L._newName = newName
          }
    resp <- request L.SMethod_TextDocumentRename params
    pure $ case resp of
      L.TResponseMessage _ _ (Right (L.InL edit)) -> fromWorkspaceEditResult edit
      L.TResponseMessage _ _ (Right (L.InR L.Null)) -> WorkspaceEdit Map.empty
      L.TResponseMessage _ _ (Left _) -> WorkspaceEdit Map.empty

  Completion doc pos -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspPos = toPosition pos
    -- getCompletions returns [CompletionItem]
    items <- getCompletions lspDoc lspPos
    pure $ map fromCompletionItem items

  WorkspaceSymbol query -> sendM $ executeSession session $ do
    -- lsp-test doesn't have a direct getWorkspaceSymbols
    -- Use the low-level request API
    let params = L.WorkspaceSymbolParams
          { L._workDoneToken = Nothing
          , L._partialResultToken = Nothing
          , L._query = query
          }
    resp <- Language.LSP.Test.request L.SMethod_WorkspaceSymbol params
    pure $ case resp of
      L.TResponseMessage _ _ (Right result) -> fromSymbolsResult result
      L.TResponseMessage _ _ (Left _) -> []

  DocumentSymbol doc -> sendM $ executeSession session $ do
    -- lsp-test doesn't have a direct getDocumentSymbols
    -- Use the low-level request API
    let lspDoc = toTextDocumentId doc
        params = L.DocumentSymbolParams
          { L._workDoneToken = Nothing
          , L._partialResultToken = Nothing
          , L._textDocument = lspDoc
          }
    resp <- Language.LSP.Test.request L.SMethod_TextDocumentDocumentSymbol params
    pure $ case resp of
      L.TResponseMessage _ _ (Right result) -> fromDocumentSymbolResult result
      L.TResponseMessage _ _ (Left _) -> []

  PrepareCallHierarchy doc pos -> sendM $ executeSession session $ do
    let lspDoc = toTextDocumentId doc
        lspPos = toPosition pos
        params = L.CallHierarchyPrepareParams
          { L._workDoneToken = Nothing
          , L._textDocument = lspDoc
          , L._position = lspPos
          }
    resp <- Language.LSP.Test.request L.SMethod_TextDocumentPrepareCallHierarchy params
    pure $ case resp of
      L.TResponseMessage _ _ (Right result) -> case result of
        L.InL items -> map fromCallHierarchyItem items
        L.InR L.Null -> []
      L.TResponseMessage _ _ (Left _) -> []

  OutgoingCalls item -> sendM $ executeSession session $ do
    let lspItem = toCallHierarchyItem item
        params = L.CallHierarchyOutgoingCallsParams
          { L._workDoneToken = Nothing
          , L._partialResultToken = Nothing
          , L._item = lspItem
          }
    resp <- Language.LSP.Test.request L.SMethod_CallHierarchyOutgoingCalls params
    pure $ case resp of
      L.TResponseMessage _ _ (Right result) -> case result of
        L.InL items -> map fromCallHierarchyOutgoingCall items
        L.InR L.Null -> []
      L.TResponseMessage _ _ (Left _) -> []

  GetIndexingState ->
    -- Read the indexing state from the session's TVar (backward compatible)
    sendM $ getSessionIndexingState session

  GetIndexingInfo ->
    -- Read full indexing information from the session's TVar
    sendM $ getSessionIndexingInfo session


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE CONVERSIONS: Our types -> lsp-types
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a file:// URI to a file path for openDoc.
--
-- openDoc expects a FilePath (relative or absolute), not a URI.
-- This strips the "file://" prefix if present.
uriToFilePath :: Text -> FilePath
uriToFilePath uri
  | "file://" `T.isPrefixOf` uri = T.unpack $ T.drop 7 uri
  | otherwise = T.unpack uri

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

toSymbolKind :: SymbolKind -> L.SymbolKind
toSymbolKind k = case k of
  SKFile -> L.SymbolKind_File
  SKModule -> L.SymbolKind_Module
  SKNamespace -> L.SymbolKind_Namespace
  SKPackage -> L.SymbolKind_Package
  SKClass -> L.SymbolKind_Class
  SKMethod -> L.SymbolKind_Method
  SKProperty -> L.SymbolKind_Property
  SKField -> L.SymbolKind_Field
  SKConstructor -> L.SymbolKind_Constructor
  SKEnum -> L.SymbolKind_Enum
  SKInterface -> L.SymbolKind_Interface
  SKFunction -> L.SymbolKind_Function
  SKVariable -> L.SymbolKind_Variable
  SKConstant -> L.SymbolKind_Constant
  SKString -> L.SymbolKind_String
  SKNumber -> L.SymbolKind_Number
  SKBoolean -> L.SymbolKind_Boolean
  SKArray -> L.SymbolKind_Array
  SKObject -> L.SymbolKind_Object
  SKKey -> L.SymbolKind_Key
  SKNull -> L.SymbolKind_Null
  SKEnumMember -> L.SymbolKind_EnumMember
  SKStruct -> L.SymbolKind_Struct
  SKEvent -> L.SymbolKind_Event
  SKOperator -> L.SymbolKind_Operator
  SKTypeParameter -> L.SymbolKind_TypeParameter

toCallHierarchyItem :: CallHierarchyItem -> L.CallHierarchyItem
toCallHierarchyItem chi = L.CallHierarchyItem
  { L._name = chi.chiName
  , L._kind = toSymbolKind chi.chiKind
  , L._tags = Nothing
  , L._detail = Nothing
  , L._uri = L.Uri chi.chiUri
  , L._range = toRange chi.chiRange
  , L._selectionRange = toRange chi.chiSelectionRange
  , L._data_ = Nothing
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

-- lsp-test's getHover returns Maybe Hover directly
fromHoverResult :: Maybe L.Hover -> Maybe HoverInfo
fromHoverResult Nothing = Nothing
fromHoverResult (Just h) = Just HoverInfo
  { hoverContents = extractMarkupContent h._contents
  , hoverRange = fromRange <$> h._range
  }

extractMarkupContent :: L.MarkupContent L.|? (L.MarkedString L.|? [L.MarkedString]) -> Text
extractMarkupContent content = case content of
  L.InL mc -> mc._value
  L.InR msOrList -> case msOrList of
    L.InL ms -> markedStringToText ms
    L.InR msList -> T.intercalate "\n" (map markedStringToText msList)

markedStringToText :: L.MarkedString -> Text
markedStringToText (L.MarkedString inner) = case inner of
  L.InL t -> t
  L.InR mswl -> mswl._value

-- lsp-test's getDefinitions returns Definition |? ([DefinitionLink] |? Null)
-- Definition = Location |? [Location]
-- DefinitionLink = newtype LocationLink
fromDefinitionResult :: (L.Definition L.|? ([L.DefinitionLink] L.|? L.Null)) -> [Location]
fromDefinitionResult result = case result of
  L.InL def -> fromDefinition def
  L.InR linksOrNull -> case linksOrNull of
    L.InL links -> map fromDefinitionLink links
    L.InR L.Null -> []
  where
    fromDefinition :: L.Definition -> [Location]
    fromDefinition d = case d of
      L.Definition (L.InL loc) -> [fromLocation loc]
      L.Definition (L.InR locs) -> map fromLocation locs

    fromDefinitionLink :: L.DefinitionLink -> Location
    fromDefinitionLink (L.DefinitionLink link) = fromLocationLink link

fromLocationLink :: L.LocationLink -> Location
fromLocationLink link = Location
  { locUri = let L.Uri u = link._targetUri in u
  , locRange = fromRange link._targetRange
  }

-- lsp-test's getCodeActions returns [Command |? CodeAction]
fromCodeActionsResult :: [L.Command L.|? L.CodeAction] -> [CodeAction]
fromCodeActionsResult items = concatMap fromCommandOrAction items

fromCommandOrAction :: L.Command L.|? L.CodeAction -> [CodeAction]
fromCommandOrAction cmdOrAction = case cmdOrAction of
  L.InL _cmd -> []  -- Skip commands
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
  L.CodeActionKind_Empty -> OtherKind ""
  _ -> OtherKind ""  -- Handle any new constructors

-- lsp-test's rename returns WorkspaceEdit directly
fromWorkspaceEditResult :: L.WorkspaceEdit -> WorkspaceEdit
fromWorkspaceEditResult we = fromWorkspaceEditMaybe (Just we)
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

fromCompletionItem :: L.CompletionItem -> CompletionItem
fromCompletionItem ci = CompletionItem
  { ciLabel = ci._label
  , ciKind = fromCompletionItemKind <$> ci._kind
  , ciDetail = ci._detail
  , ciDocumentation = extractDocumentation <$> ci._documentation
  , ciInsertText = ci._insertText
  }

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

-- Result type is [SymbolInformation] |? ([WorkspaceSymbol] |? Null)
fromSymbolsResult :: [L.SymbolInformation] L.|? ([L.WorkspaceSymbol] L.|? L.Null) -> [SymbolInformation]
fromSymbolsResult result = case result of
  L.InL infos -> map fromSymbolInfo infos
  L.InR wsOrNull -> case wsOrNull of
    L.InL wsSymbols -> map fromWorkspaceSymbol wsSymbols
    L.InR L.Null -> []

fromSymbolInfo :: L.SymbolInformation -> SymbolInformation
fromSymbolInfo si = SymbolInformation
  { siName = si._name
  , siKind = fromSymbolKind si._kind
  , siLocation = fromLocation si._location
  , siContainer = si._containerName
  }

fromWorkspaceSymbol :: L.WorkspaceSymbol -> SymbolInformation
fromWorkspaceSymbol ws = SymbolInformation
  { siName = ws._name
  , siKind = fromSymbolKind ws._kind
  , siLocation = extractLocationFromWS ws._location
  , siContainer = ws._containerName
  }

extractLocationFromWS :: L.Location L.|? L.LocationUriOnly -> Location
extractLocationFromWS loc = case loc of
  L.InL l -> fromLocation l
  L.InR (L.LocationUriOnly uri) -> Location
    { locUri = let L.Uri u = uri in u
    , locRange = Range (Position 0 0) (Position 0 0)
    }

fromSymbolKind :: L.SymbolKind -> SymbolKind
fromSymbolKind k = case k of
  L.SymbolKind_File -> SKFile
  L.SymbolKind_Module -> SKModule
  L.SymbolKind_Namespace -> SKNamespace
  L.SymbolKind_Package -> SKPackage
  L.SymbolKind_Class -> SKClass
  L.SymbolKind_Method -> SKMethod
  L.SymbolKind_Property -> SKProperty
  L.SymbolKind_Field -> SKField
  L.SymbolKind_Constructor -> SKConstructor
  L.SymbolKind_Enum -> SKEnum
  L.SymbolKind_Interface -> SKInterface
  L.SymbolKind_Function -> SKFunction
  L.SymbolKind_Variable -> SKVariable
  L.SymbolKind_Constant -> SKConstant
  L.SymbolKind_String -> SKString
  L.SymbolKind_Number -> SKNumber
  L.SymbolKind_Boolean -> SKBoolean
  L.SymbolKind_Array -> SKArray
  L.SymbolKind_Object -> SKObject
  L.SymbolKind_Key -> SKKey
  L.SymbolKind_Null -> SKNull
  L.SymbolKind_EnumMember -> SKEnumMember
  L.SymbolKind_Struct -> SKStruct
  L.SymbolKind_Event -> SKEvent
  L.SymbolKind_Operator -> SKOperator
  L.SymbolKind_TypeParameter -> SKTypeParameter

-- Result type is [SymbolInformation] |? ([DocumentSymbol] |? Null)
fromDocumentSymbolResult :: [L.SymbolInformation] L.|? ([L.DocumentSymbol] L.|? L.Null) -> [SymbolInformation]
fromDocumentSymbolResult result = case result of
  L.InL infos -> map fromSymbolInfo infos
  L.InR docSymbolsOrNull -> case docSymbolsOrNull of
    L.InL docSymbols -> concatMap flattenDocumentSymbol docSymbols
    L.InR L.Null -> []

-- | Flatten hierarchical DocumentSymbol to SymbolInformation list.
--
-- DocumentSymbol has optional children (nested symbols). We flatten
-- by including the parent and recursively flattening children.
flattenDocumentSymbol :: L.DocumentSymbol -> [SymbolInformation]
flattenDocumentSymbol ds =
  let parent = SymbolInformation
        { siName = ds._name
        , siKind = fromSymbolKind ds._kind
        , siLocation = Location
            { locUri = ""  -- DocumentSymbol doesn't include URI
            , locRange = fromRange ds._range
            }
        , siContainer = Nothing  -- Could extract from parent context if needed
        }
      children = maybe [] (concatMap flattenDocumentSymbol) ds._children
  in parent : children

fromCallHierarchyItem :: L.CallHierarchyItem -> CallHierarchyItem
fromCallHierarchyItem chi = CallHierarchyItem
  { chiName = chi._name
  , chiKind = fromSymbolKind chi._kind
  , chiUri = let L.Uri u = chi._uri in u
  , chiRange = fromRange chi._range
  , chiSelectionRange = fromRange chi._selectionRange
  }

fromCallHierarchyOutgoingCall :: L.CallHierarchyOutgoingCall -> CallHierarchyOutgoingCall
fromCallHierarchyOutgoingCall choc = CallHierarchyOutgoingCall
  { chocTo = fromCallHierarchyItem choc._to
  , chocFromRanges = map fromRange choc._fromRanges
  }


-- ════════════════════════════════════════════════════════════════════════════
-- PROGRESS TOKEN CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert LSP ProgressToken to Text for debugging.
--
-- ProgressToken is Int32 |? Text in lsp-types. We stringify both variants
-- to capture what HLS is reporting during indexing.
showProgressToken :: L.ProgressToken -> Text
showProgressToken (L.ProgressToken tok) = case tok of
  L.InL n  -> T.pack (show n)  -- Int32 token
  L.InR t  -> t                -- Text token
