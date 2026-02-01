-- | Template Haskell for generating WASM exports.
--
-- Generates the three required WASM entry points.
--
-- Two approaches are supported:
--
-- * Type-level list: @$(mkWasmExports ''MyToolList)@
-- * Record-based: @$(mkWasmExportsRecord 'myToolsHandler 'myToolsSchema)@
module ExoMonad.Guest.Tool.TH
  ( -- * Type-level list (legacy)
    mkWasmExports,

    -- * Record-based (new)
    mkWasmExportsRecord,

    -- * Lower-level handlers (used by generated code)
    mcpHandler,
    listHandler,
    mcpHandlerRecord,
    listHandlerRecord,
    hookHandler,
    wrapHandler,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import Language.Haskell.TH

import ExoMonad.Guest.Tool.Class (DispatchTools (..), MCPCallOutput (..), toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Types (HookInput, HookOutput, MCPCallInput (..), allowResponse)

-- ============================================================================
-- Template Haskell
-- ============================================================================

-- | Generate WASM exports for a tool list type.
--
-- Usage:
--
-- > $(mkWasmExports ''MyToolList)
--
-- Generates:
--
-- > foreign export ccall handle_mcp_call :: IO CInt
-- > handle_mcp_call :: IO CInt
-- > handle_mcp_call = wrapHandler $ mcpHandler @MyToolList
-- >
-- > foreign export ccall handle_list_tools :: IO CInt
-- > handle_list_tools :: IO CInt
-- > handle_list_tools = wrapHandler $ listHandler @MyToolList
-- >
-- > foreign export ccall handle_pre_tool_use :: IO CInt
-- > handle_pre_tool_use :: IO CInt
-- > handle_pre_tool_use = wrapHandler hookHandler
mkWasmExports :: Name -> Q [Dec]
mkWasmExports toolsName = do
  -- Get the type from the name
  let toolsType = ConT toolsName

  -- Create names for the generated functions
  let mcpName = mkName "handle_mcp_call"
  let listName = mkName "handle_list_tools"
  let hookName = mkName "handle_pre_tool_use"

  -- Generate foreign export declarations
  let mcpExport = ForeignD $ ExportF CCall "handle_mcp_call" mcpName (AppT (ConT ''IO) (ConT ''CInt))
  let listExport = ForeignD $ ExportF CCall "handle_list_tools" listName (AppT (ConT ''IO) (ConT ''CInt))
  let hookExport = ForeignD $ ExportF CCall "handle_pre_tool_use" hookName (AppT (ConT ''IO) (ConT ''CInt))

  -- Generate handle_mcp_call = wrapHandler $ mcpHandler @ToolsType
  let mcpSig = SigD mcpName (AppT (ConT ''IO) (ConT ''CInt))
  let mcpBody = AppE (VarE 'wrapHandler) (AppTypeE (VarE 'mcpHandler) toolsType)
  let mcpDef = ValD (VarP mcpName) (NormalB mcpBody) []

  -- Generate handle_list_tools = wrapHandler $ listHandler @ToolsType
  let listSig = SigD listName (AppT (ConT ''IO) (ConT ''CInt))
  let listBody = AppE (VarE 'wrapHandler) (AppTypeE (VarE 'listHandler) toolsType)
  let listDef = ValD (VarP listName) (NormalB listBody) []

  -- Generate handle_pre_tool_use = wrapHandler hookHandler
  let hookSig = SigD hookName (AppT (ConT ''IO) (ConT ''CInt))
  let hookBody = AppE (VarE 'wrapHandler) (VarE 'hookHandler)
  let hookDef = ValD (VarP hookName) (NormalB hookBody) []

  pure
    [ mcpExport
    , mcpSig
    , mcpDef
    , listExport
    , listSig
    , listDef
    , hookExport
    , hookSig
    , hookDef
    ]

-- | Generate WASM exports for a record-based tool set.
--
-- Usage:
--
-- > $(mkWasmExportsRecord 'devToolsHandler)
--
-- Generates:
--
-- > foreign export ccall handle_mcp_call :: IO CInt
-- > handle_mcp_call :: IO CInt
-- > handle_mcp_call = wrapHandler $ mcpHandlerRecord devToolsHandler
-- >
-- > foreign export ccall handle_list_tools :: IO CInt
-- > handle_list_tools :: IO CInt
-- > handle_list_tools = wrapHandler $ listHandlerRecord @DevTools
-- >
-- > foreign export ccall handle_pre_tool_use :: IO CInt
-- > handle_pre_tool_use :: IO CInt
-- > handle_pre_tool_use = wrapHandler hookHandler
mkWasmExportsRecord :: Name -> Q [Dec]
mkWasmExportsRecord handlerName = do
  -- Get the handler's type to extract the tools type
  info <- reify handlerName
  toolsType <- case info of
    VarI _ ty _ -> extractToolsType ty
    _ -> fail $ "Expected a value, got: " ++ show info

  -- Create names for the generated functions
  let mcpName = mkName "handle_mcp_call"
  let listName = mkName "handle_list_tools"
  let hookName = mkName "handle_pre_tool_use"

  -- Generate foreign export declarations
  let mcpExport = ForeignD $ ExportF CCall "handle_mcp_call" mcpName (AppT (ConT ''IO) (ConT ''CInt))
  let listExport = ForeignD $ ExportF CCall "handle_list_tools" listName (AppT (ConT ''IO) (ConT ''CInt))
  let hookExport = ForeignD $ ExportF CCall "handle_pre_tool_use" hookName (AppT (ConT ''IO) (ConT ''CInt))

  -- Generate handle_mcp_call = wrapHandler $ mcpHandlerRecord handlerValue
  let mcpSig = SigD mcpName (AppT (ConT ''IO) (ConT ''CInt))
  let mcpBody = AppE (VarE 'wrapHandler) (AppE (VarE 'mcpHandlerRecord) (VarE handlerName))
  let mcpDef = ValD (VarP mcpName) (NormalB mcpBody) []

  -- Generate handle_list_tools = wrapHandler $ listHandlerRecord @ToolsType
  let listSig = SigD listName (AppT (ConT ''IO) (ConT ''CInt))
  let listBody = AppE (VarE 'wrapHandler) (AppTypeE (VarE 'listHandlerRecord) toolsType)
  let listDef = ValD (VarP listName) (NormalB listBody) []

  -- Generate handle_pre_tool_use = wrapHandler hookHandler
  let hookSig = SigD hookName (AppT (ConT ''IO) (ConT ''CInt))
  let hookBody = AppE (VarE 'wrapHandler) (VarE 'hookHandler)
  let hookDef = ValD (VarP hookName) (NormalB hookBody) []

  pure
    [ mcpExport
    , mcpSig
    , mcpDef
    , listExport
    , listSig
    , listDef
    , hookExport
    , hookSig
    , hookDef
    ]

-- | Extract the tools type constructor from a handler type.
--
-- Given @DevTools AsHandler@, extracts @DevTools@.
extractToolsType :: Type -> Q Type
extractToolsType (AppT toolsType (ConT asHandler))
  | asHandler == ''AsHandler = pure toolsType
extractToolsType ty = fail $ "Expected 'tools AsHandler', got: " ++ show ty

-- ============================================================================
-- Handler implementations
-- ============================================================================

-- | MCP call handler - dispatches to tools based on the type list.
mcpHandler :: forall tools. (DispatchTools tools) => IO CInt
mcpHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let resp = MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 1
    Right mcpCall -> do
      resp <- dispatch @tools (toolName mcpCall) (toolArgs mcpCall)
      output (BSL.toStrict $ Aeson.encode resp)
      if success resp then pure 0 else pure 1

-- | List tools handler - returns all tool definitions for the type list.
listHandler :: forall tools. (DispatchTools tools) => IO CInt
listHandler = do
  let tools = map toMCPFormat (toolDefs @tools)
  output (BSL.toStrict $ Aeson.encode tools)
  pure 0

-- ============================================================================
-- Record-based handler implementations
-- ============================================================================

-- | MCP call handler - dispatches to tools based on a record.
mcpHandlerRecord :: forall tools. (DispatchRecord tools) => tools AsHandler -> IO CInt
mcpHandlerRecord handlers = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let resp = MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 1
    Right mcpCall -> do
      resp <- dispatchRecord handlers (toolName mcpCall) (toolArgs mcpCall)
      output (BSL.toStrict $ Aeson.encode resp)
      if success resp then pure 0 else pure 1

-- | List tools handler - returns all tool definitions for a record type.
listHandlerRecord :: forall tools. (ReifyRecord tools) => IO CInt
listHandlerRecord = do
  let tools = map toMCPFormat (reifyToolDefs (Proxy @tools))
  output (BSL.toStrict $ Aeson.encode tools)
  pure 0

-- ============================================================================
-- Hook handler
-- ============================================================================

-- | Hook handler - handles PreToolUse hooks.
-- Currently allows all tool uses.
hookHandler :: IO CInt
hookHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let errResp = Aeson.object ["error" Aeson..= ("Parse error: " ++ err)]
      output (BSL.toStrict $ Aeson.encode errResp)
      pure 1
    Right (_hookInput :: HookInput) -> do
      -- For now, always allow
      let resp = allowResponse Nothing
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0

-- | Wrap a handler with exception handling.
wrapHandler :: IO CInt -> IO CInt
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      let errJson = Aeson.encode $ Aeson.object ["error" Aeson..= show err]
      output (BSL.toStrict errJson)
      pure 1
