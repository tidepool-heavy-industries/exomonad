-- | LSP effect executor.
--
-- Interprets 'LSP' effects by communicating with a language server
-- via 'LSPClient'.
--
-- == Example Usage
--
-- @
-- import Tidepool.LSP.Executor
-- import Tidepool.Effects.LSP
--
-- main = do
--   client <- startLSPClient "haskell-language-server-wrapper" ["--lsp"]
--   result <- runLSP client myAgentLogic
--   stopLSPClient client
-- @
--
module Tidepool.LSP.Executor
  ( -- * Executor
    runLSP

    -- * Re-exports
  , module Tidepool.LSP.Client
  ) where

import Control.Monad.Freer (Eff, LastMember, sendM, interpret)
import Data.Aeson
import Data.Aeson.Key (Key, toText, fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Tidepool.Effects.LSP
import Tidepool.LSP.Client
import Tidepool.LSP.Protocol (hoverMethod, referencesMethod, definitionMethod, codeActionMethod, renameMethod, completionMethod)


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LSP effects by communicating with a language server.
--
-- The client must be initialized before calling this function.
runLSP :: LastMember IO effs => LSPClient -> Eff (LSP ': effs) a -> Eff effs a
runLSP client = interpret $ \case
  Diagnostics doc -> sendM $ getDiagnostics client doc
  Hover doc pos -> sendM $ getHover client doc pos
  References doc pos -> sendM $ getReferences client doc pos
  Definition doc pos -> sendM $ getDefinition client doc pos
  CodeActions doc rng -> sendM $ getCodeActions client doc rng
  Rename doc pos newName -> sendM $ doRename client doc pos newName
  Completion doc pos -> sendM $ getCompletion client doc pos


-- ════════════════════════════════════════════════════════════════════════════
-- LSP OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get diagnostics for a document.
--
-- Note: LSP doesn't have a request for this - diagnostics come via
-- notifications. For now, return empty list.
-- TODO: Track diagnostics from notifications.
getDiagnostics :: LSPClient -> TextDocumentIdentifier -> IO [Diagnostic]
getDiagnostics _client _doc = do
  -- Diagnostics are pushed via notifications, not requested
  -- We'd need to track them in LSPClient state
  pure []

-- | Get hover information.
getHover :: LSPClient -> TextDocumentIdentifier -> Position -> IO (Maybe HoverInfo)
getHover client doc pos = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "position" .= positionToLSP pos
        ]
  result <- sendRequest client hoverMethod params
  case result of
    Left _ -> pure Nothing
    Right val -> pure $ parseHoverResult val

-- | Get references to a symbol.
getReferences :: LSPClient -> TextDocumentIdentifier -> Position -> IO [Location]
getReferences client doc pos = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "position" .= positionToLSP pos
        , "context" .= object ["includeDeclaration" .= True]
        ]
  result <- sendRequest client referencesMethod params
  case result of
    Left _ -> pure []
    Right val -> pure $ parseLocations val

-- | Get definition of a symbol.
getDefinition :: LSPClient -> TextDocumentIdentifier -> Position -> IO [Location]
getDefinition client doc pos = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "position" .= positionToLSP pos
        ]
  result <- sendRequest client definitionMethod params
  case result of
    Left _ -> pure []
    Right val -> pure $ parseLocations val

-- | Get code actions for a range.
getCodeActions :: LSPClient -> TextDocumentIdentifier -> Range -> IO [CodeAction]
getCodeActions client doc rng = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "range" .= rangeToLSP rng
        , "context" .= object ["diagnostics" .= ([] :: [Value])]
        ]
  result <- sendRequest client codeActionMethod params
  case result of
    Left _ -> pure []
    Right val -> pure $ parseCodeActions val

-- | Rename a symbol.
doRename :: LSPClient -> TextDocumentIdentifier -> Position -> Text -> IO WorkspaceEdit
doRename client doc pos newName = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "position" .= positionToLSP pos
        , "newName" .= newName
        ]
  result <- sendRequest client renameMethod params
  case result of
    Left _ -> pure $ WorkspaceEdit Map.empty
    Right val -> pure $ parseWorkspaceEdit val

-- | Get completion suggestions.
getCompletion :: LSPClient -> TextDocumentIdentifier -> Position -> IO [CompletionItem]
getCompletion client doc pos = do
  let params = object
        [ "textDocument" .= object ["uri" .= doc.tdiUri]
        , "position" .= positionToLSP pos
        ]
  result <- sendRequest client completionMethod params
  case result of
    Left _ -> pure []
    Right val -> pure $ parseCompletionItems val


-- ════════════════════════════════════════════════════════════════════════════
-- CONVERSIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert Position to LSP JSON format.
positionToLSP :: Position -> Value
positionToLSP pos = object
  [ "line" .= pos.posLine
  , "character" .= pos.posCharacter
  ]

-- | Convert Range to LSP JSON format.
rangeToLSP :: Range -> Value
rangeToLSP rng = object
  [ "start" .= positionToLSP rng.rangeStart
  , "end" .= positionToLSP rng.rangeEnd
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- PARSING HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse hover result from LSP response.
parseHoverResult :: Value -> Maybe HoverInfo
parseHoverResult val = case val of
  Null -> Nothing
  Object o -> do
    contents <- case lookupKey "contents" o of
      Just (String s) -> Just s
      Just (Object c) -> case lookupKey "value" c of
        Just (String s) -> Just s
        _ -> Nothing
      Just (Array arr) -> case foldr (:) [] arr of
        [] -> Nothing
        (x:_) -> case x of
          String s -> Just s
          Object c -> case lookupKey "value" c of
            Just (String s) -> Just s
            _ -> Nothing
          _ -> Nothing
      _ -> Nothing
    pure HoverInfo
      { hoverContents = contents
      , hoverRange = Nothing  -- TODO: Parse range if present
      }
  _ -> Nothing

-- | Parse locations from LSP response.
parseLocations :: Value -> [Location]
parseLocations val = case val of
  Null -> []
  Array arr -> mapMaybe parseLocation (toList arr)
  Object _ -> maybeToList $ parseLocation val  -- Single location
  _ -> []
  where
    toList = foldr (:) []

-- | Parse a single location.
parseLocation :: Value -> Maybe Location
parseLocation = \case
  Object o -> do
    uri <- case lookupKey "uri" o of
      Just (String s) -> Just s
      _ -> Nothing
    rng <- case lookupKey "range" o of
      Just r -> parseRange r
      _ -> Nothing
    pure Location { locUri = uri, locRange = rng }
  _ -> Nothing

-- | Parse a range.
parseRange :: Value -> Maybe Range
parseRange = \case
  Object o -> do
    start <- case lookupKey "start" o of
      Just p -> parsePosition p
      _ -> Nothing
    end <- case lookupKey "end" o of
      Just p -> parsePosition p
      _ -> Nothing
    pure Range { rangeStart = start, rangeEnd = end }
  _ -> Nothing

-- | Parse a position.
parsePosition :: Value -> Maybe Position
parsePosition = \case
  Object o -> do
    line <- case lookupKey "line" o of
      Just (Number n) -> Just (round n)
      _ -> Nothing
    char <- case lookupKey "character" o of
      Just (Number n) -> Just (round n)
      _ -> Nothing
    pure Position { posLine = line, posCharacter = char }
  _ -> Nothing

-- | Parse code actions.
parseCodeActions :: Value -> [CodeAction]
parseCodeActions = \case
  Array arr -> mapMaybe parseCodeAction (foldr (:) [] arr)
  _ -> []

-- | Parse a single code action.
parseCodeAction :: Value -> Maybe CodeAction
parseCodeAction = \case
  Object o -> Just CodeAction
    { caTitle = case lookupKey "title" o of
        Just (String s) -> s
        _ -> "Unknown"
    , caKind = Nothing  -- TODO: Parse kind
    , caEdit = Nothing  -- TODO: Parse edit
    , caCommand = case lookupKey "command" o of
        Just (String s) -> Just s
        _ -> Nothing
    }
  _ -> Nothing

-- | Parse workspace edit.
parseWorkspaceEdit :: Value -> WorkspaceEdit
parseWorkspaceEdit = \case
  Object o -> case lookupKey "changes" o of
    Just (Object changes) ->
      let edits = foldr (\(k, v) acc ->
                          case parseTextEdits v of
                            [] -> acc
                            es -> Map.insert (toText k) es acc
                        ) Map.empty (KM.toList changes)
      in WorkspaceEdit edits
    _ -> WorkspaceEdit Map.empty
  _ -> WorkspaceEdit Map.empty

-- | Parse text edits.
parseTextEdits :: Value -> [TextEdit]
parseTextEdits = \case
  Array arr -> mapMaybe parseTextEdit (foldr (:) [] arr)
  _ -> []

-- | Parse a single text edit.
parseTextEdit :: Value -> Maybe TextEdit
parseTextEdit = \case
  Object o -> do
    rng <- case lookupKey "range" o of
      Just r -> parseRange r
      _ -> Nothing
    newText <- case lookupKey "newText" o of
      Just (String s) -> Just s
      _ -> Nothing
    pure TextEdit { teRange = rng, teNewText = newText }
  _ -> Nothing

-- | Parse completion items.
parseCompletionItems :: Value -> [CompletionItem]
parseCompletionItems = \case
  Object o -> case lookupKey "items" o of
    Just (Array arr) -> mapMaybe parseCompletionItem (foldr (:) [] arr)
    _ -> []
  Array arr -> mapMaybe parseCompletionItem (foldr (:) [] arr)
  _ -> []

-- | Parse a single completion item.
parseCompletionItem :: Value -> Maybe CompletionItem
parseCompletionItem = \case
  Object o -> Just CompletionItem
    { ciLabel = case lookupKey "label" o of
        Just (String s) -> s
        _ -> ""
    , ciKind = Nothing  -- TODO: Parse kind
    , ciDetail = case lookupKey "detail" o of
        Just (String s) -> Just s
        _ -> Nothing
    , ciDocumentation = case lookupKey "documentation" o of
        Just (String s) -> Just s
        _ -> Nothing
    , ciInsertText = case lookupKey "insertText" o of
        Just (String s) -> Just s
        _ -> Nothing
    }
  _ -> Nothing


-- | Lookup a key in an aeson Object.
lookupKey :: Text -> Object -> Maybe Value
lookupKey k o = KM.lookup (fromText k) o


-- | Maybe to list helper.
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- | Map maybe helper.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y -> y : mapMaybe f xs
