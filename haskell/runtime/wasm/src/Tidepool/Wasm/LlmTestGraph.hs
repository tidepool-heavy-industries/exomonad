{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Test graph for proving LLM effect works over WASM boundary.
--
-- A minimal graph that:
-- 1. Takes a Text (user message)
-- 2. Makes an LLM call (yields to TypeScript)
-- 3. Returns the LLM response as Text
--
-- This proves the LLM yield/resume cycle works over the WASM boundary.
module Tidepool.Wasm.LlmTestGraph
  ( -- * Graph Type
    LlmTestGraph(..)
    -- * WASM Handler
  , echoHandlerWasm
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)

import Tidepool.Wasm.Effect (WasmM, llmComplete)
import Tidepool.Wasm.Error (formatWasmError)


-- | LLM test graph: Text in, Text out.
--
-- Structure:
--   Entry(Text) → echo → Exit(Text)
--
-- The echo node makes an LLM call (yielding to TypeScript) then exits with the response.
data LlmTestGraph mode = LlmTestGraph
  { entry :: mode :- G.EntryNode Text
  , echo  :: mode :- G.LogicNode :@ Input Text :@ UsesEffects '[Goto Exit Text]
  , exit  :: mode :- G.ExitNode Text
  }
  deriving Generic


-- | WASM handler for the echo node.
--
-- This handler:
-- 1. Makes an LLM call with "Echo back: {input}" (yields to TypeScript)
-- 2. Handles errors or extracts the response text from the JSON result
-- 3. Returns gotoExit with the response (or error message)
--
-- The LLM effect causes execution to suspend. TypeScript executes the LLM call,
-- then calls resume with the result. Execution continues and returns the GotoChoice.
echoHandlerWasm :: Text -> WasmM (GotoChoice '[To Exit Text])
echoHandlerWasm userMsg = do
  result <- llmComplete
    "echo"                              -- node name
    "You are an echo bot. Echo back whatever the user says."  -- system prompt
    ("Echo back: " <> userMsg)          -- user content
    Nothing                             -- no schema (free-form response)
  case result of
    Left err -> pure $ gotoExit ("Error: " <> formatWasmError err)
    Right response -> pure $ gotoExit (extractResponseText response)


-- | Extract response text from LLM result.
--
-- The LLM result is a JSON object. We look for common patterns:
-- - { "response": "..." }
-- - { "text": "..." }
-- - { "content": "..." }
-- - Or just return the JSON as text
extractResponseText :: Value -> Text
extractResponseText (Object obj) =
  case (KM.lookup "response" obj, KM.lookup "text" obj, KM.lookup "content" obj) of
    (Just (String s), _, _) -> s
    (_, Just (String s), _) -> s
    (_, _, Just (String s)) -> s
    _ -> T.pack (show obj)
extractResponseText (String s) = s
extractResponseText v = T.pack (show v)
