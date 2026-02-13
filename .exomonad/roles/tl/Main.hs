{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Foreign.C.Types (CInt(..))
import ExoMonad.Guest.Tool.Runtime (hookHandler, listHandlerRecord, mcpHandlerRecord, resumeHandler, wrapHandler)
import Role (config, Tools)
import ExoMonad.Types (RoleConfig(..))

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall resume :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ mcpHandlerRecord (tools config)

resume :: IO CInt
resume = wrapHandler resumeHandler

handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ listHandlerRecord @Tools

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler $ hookHandler (hooks config)

main :: IO ()
main = pure ()
