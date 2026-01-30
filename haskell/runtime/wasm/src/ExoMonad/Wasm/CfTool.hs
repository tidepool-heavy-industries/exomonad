-- | CF AI tool schema types (re-exported from "ExoMonad.Tool.Wire").
--
-- Cloudflare Workers AI uses OpenAI-compatible tool format:
-- @{type: "function", function: {name, description, parameters}}@.
-- This format works with llama-3.3, llama-4-scout, and other CF AI models.
--
-- The wire types are now defined in "ExoMonad.Tool.Wire" and re-exported here
-- for backward compatibility.
--
-- Example:
--
-- @
-- askUserTool :: CfTool
-- askUserTool = CfTool
--   { ctName = "ask_user"
--   , ctDescription = "Ask the user a question"
--   , ctParameters = CfObjectSchema
--       { cosProperties = Map.fromList
--           [ ("question", CfString "The question to ask")
--           ]
--       , cosRequired = ["question"]
--       }
--   }
-- @
--
-- Serializes to:
--
-- @
-- {
--   "name": "ask_user",
--   "description": "Ask the user a question",
--   "parameters": {
--     "type": "object",
--     "properties": {
--       "question": { "type": "string", "description": "The question to ask" }
--     },
--     "required": ["question"]
--   }
-- }
-- @
module ExoMonad.Wasm.CfTool
  ( -- * Tool Schema (re-exported from "ExoMonad.Tool.Wire")
    CfTool (..),
    CfObjectSchema (..),
    CfProperty (..),
    cfToolToValue,
  )
where

-- All types now defined in exomonad-core and re-exported here
import ExoMonad.Tool.Wire
  ( CfObjectSchema (..),
    CfProperty (..),
    CfTool (..),
    cfToolToValue,
  )
