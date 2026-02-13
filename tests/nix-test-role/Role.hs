{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types (CInt(..))

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall handle_test_call :: IO CInt
foreign export ccall resume :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = pure 0

handle_list_tools :: IO CInt
handle_list_tools = pure 0

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = pure 0

handle_test_call :: IO CInt
handle_test_call = pure 0

resume :: IO CInt
resume = pure 0

main :: IO ()
main = pure ()
