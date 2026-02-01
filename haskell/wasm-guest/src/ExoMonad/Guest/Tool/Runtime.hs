-- | Runtime handlers for WASM exports.
--
-- Separated from TH.hs because these use Extism.PDK which cannot be
-- loaded by the TH external interpreter during cross-compilation.
module ExoMonad.Guest.Tool.Runtime
  ( mcpHandlerRecord,
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
import Data.Text qualified as T
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Types (HookInput, MCPCallInput (..), allowResponse)
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))

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
