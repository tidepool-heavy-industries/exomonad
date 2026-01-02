{-# LANGUAGE CPP #-}

-- | Unified FFI exports for the graph registry.
--
-- This module provides 4 WASM exports that take graphId as a parameter,
-- replacing the previous 4N exports (4 per graph).
--
-- = Exports
--
-- * @initialize(graphId, input)@ - Start graph execution
-- * @step(graphId, result)@ - Continue with effect result
-- * @getGraphInfo(graphId)@ - Get static graph metadata
-- * @getGraphState(graphId)@ - Get runtime state
--
-- = Usage from TypeScript
--
-- @
-- const exports = await loadWasm();
-- const output = exports.initialize("habitica", JSON.stringify({messageText: "done workout"}));
-- // ... handle effects ...
-- const next = exports.step("habitica", JSON.stringify({type: "success", value: {...}}));
-- @
module Tidepool.Wasm.Ffi.Unified
  ( -- * Unified FFI (Text interface)
    initialize
  , step
  , getGraphInfo
  , getGraphState

    -- * Testing
  , resetSession
  ) where

import Data.Text (Text)
import qualified Data.Text as T

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
#endif

-- Import the registry which has all the logic
import qualified Tidepool.Wasm.Registry as R


-- ════════════════════════════════════════════════════════════════════════════
-- TEXT INTERFACE (for native testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize a graph session.
initialize :: Text -> Text -> IO Text
initialize = R.initialize

-- | Step the current session.
step :: Text -> Text -> IO Text
step = R.step

-- | Get graph info.
getGraphInfo :: Text -> IO Text
getGraphInfo = R.getGraphInfo

-- | Get current graph state.
getGraphState :: Text -> IO Text
getGraphState = R.getGraphState

-- | Reset session (for testing).
resetSession :: IO ()
resetSession = R.resetSession


-- ════════════════════════════════════════════════════════════════════════════
-- WASM FFI EXPORTS
-- ════════════════════════════════════════════════════════════════════════════

#if defined(wasm32_HOST_ARCH)

-- WASM wrappers that convert JSString <-> Text

initialize_wasm :: JSString -> JSString -> IO JSString
initialize_wasm graphIdJs inputJs = do
  let graphId = T.pack $ fromJSString graphIdJs
      input = T.pack $ fromJSString inputJs
  result <- R.initialize graphId input
  pure $ toJSString $ T.unpack result

step_wasm :: JSString -> JSString -> IO JSString
step_wasm graphIdJs resultJs = do
  let graphId = T.pack $ fromJSString graphIdJs
      resultText = T.pack $ fromJSString resultJs
  result <- R.step graphId resultText
  pure $ toJSString $ T.unpack result

getGraphInfo_wasm :: JSString -> IO JSString
getGraphInfo_wasm graphIdJs = do
  let graphId = T.pack $ fromJSString graphIdJs
  result <- R.getGraphInfo graphId
  pure $ toJSString $ T.unpack result

getGraphState_wasm :: JSString -> IO JSString
getGraphState_wasm graphIdJs = do
  let graphId = T.pack $ fromJSString graphIdJs
  result <- R.getGraphState graphId
  pure $ toJSString $ T.unpack result

-- Foreign exports
foreign export javascript "initialize" initialize_wasm :: JSString -> JSString -> IO JSString
foreign export javascript "step" step_wasm :: JSString -> JSString -> IO JSString
foreign export javascript "getGraphInfo" getGraphInfo_wasm :: JSString -> IO JSString
foreign export javascript "getGraphState" getGraphState_wasm :: JSString -> IO JSString

#endif
