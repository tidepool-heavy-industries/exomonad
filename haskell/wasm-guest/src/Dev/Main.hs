{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

-- | Dev role WASM entry point.
module Main where

import Dev.Tools (DevTools, devToolsHandler)
import ExoMonad.Guest.Hooks.StopWorkflow (stopWorkflowHandler)
import ExoMonad.Guest.Tool.Runtime (listHandlerRecord, mcpHandlerRecord, wrapHandler)
import Foreign.C.Types (CInt (..))

-- WASM exports
foreign export ccall handle_mcp_call :: IO CInt

foreign export ccall handle_list_tools :: IO CInt

foreign export ccall handle_pre_tool_use :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ mcpHandlerRecord devToolsHandler

handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ listHandlerRecord @DevTools

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler stopWorkflowHandler

-- Required for WASM but not called (reactor model)
main :: IO ()
main = pure ()
