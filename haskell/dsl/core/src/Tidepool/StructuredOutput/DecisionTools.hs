{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate MCP decision tools from Haskell sum types.
--
-- This module enables ClaudeCode nodes to return sum types by generating
-- MCP tools where each tool corresponds to one constructor (branch).
-- Claude Code calls a tool to "select" a branch, which is more reliable
-- than parsing unstructured JSON.
--
-- = Example
--
-- @
-- data ReviewDecision
--   = Approve { approvedBy :: Text, notes :: Text }
--   | Reject { reason :: Text, suggestedFixes :: [Text] }
--   | Escalate { to :: Text, context :: Text }
--   deriving (Generic)
--
-- instance ToDecisionTools ReviewDecision
-- @
--
-- Generates MCP tools:
--
-- * @decision::approve@ with params @{ approvedBy, notes }@
-- * @decision::reject@ with params @{ reason, suggestedFixes }@
-- * @decision::escalate@ with params @{ to, context }@
--
-- = Architecture
--
-- Claude Code (in container) connects to @mantle-agent@ as an MCP server.
-- When Claude calls @decision::approve@, mantle-agent relays the call back
-- to mantle, which reports it in @SessionOutput.soToolCalls@. Haskell parses
-- the tool call back to the sum type via 'parseToolCall'.
module Tidepool.StructuredOutput.DecisionTools
  ( -- * MCP Tool Definition
    DecisionTool(..)
  , ToolCall(..)

    -- * Typeclass
  , ToDecisionTools(..)

    -- * Constants
  , decisionServerName
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Char (isUpper, toLower)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics

import Tidepool.Schema (schemaToValue, objectSchema)
import Tidepool.StructuredOutput.Class (StructuredOptions(..), defaultOptions)
import Tidepool.StructuredOutput.Generic (GStructuredProduct(..))
import Tidepool.StructuredOutput.Prefix (detectPrefix, makeStripPrefix)


-- ════════════════════════════════════════════════════════════════════════════
-- CONSTANTS
-- ════════════════════════════════════════════════════════════════════════════

-- | The MCP server name for decision tools.
--
-- All decision tools are prefixed with this server name: @decision::approve@.
decisionServerName :: Text
decisionServerName = "decision"


-- ════════════════════════════════════════════════════════════════════════════
-- WIRE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | An MCP tool definition for a sum type branch.
--
-- @
-- DecisionTool
--   { dtName = "decision::approve"
--   , dtDescription = "Select the Approve branch"
--   , dtInputSchema = { "type": "object", "properties": {...} }
--   }
-- @
data DecisionTool = DecisionTool
  { dtName        :: !Text   -- ^ Full tool name (e.g., "decision::approve")
  , dtDescription :: !Text   -- ^ Human-readable description
  , dtInputSchema :: !Value  -- ^ JSON Schema for tool input (the branch's fields)
  }
  deriving stock (Eq, Show)

instance ToJSON DecisionTool where
  toJSON t = object
    [ "name" .= t.dtName
    , "description" .= t.dtDescription
    , "inputSchema" .= t.dtInputSchema
    ]

instance FromJSON DecisionTool where
  parseJSON = Aeson.withObject "DecisionTool" $ \o -> do
    dtName <- o .: "name"
    dtDescription <- o .: "description"
    dtInputSchema <- o .: "inputSchema"
    pure DecisionTool{..}


-- | A tool call from Claude Code.
--
-- When Claude calls @decision::approve { approvedBy: "alice", notes: "LGTM" }@,
-- mantle captures this and returns it in @SessionOutput.soToolCalls@.
data ToolCall = ToolCall
  { tcName  :: !Text   -- ^ Full tool name (e.g., "decision::approve")
  , tcInput :: !Value  -- ^ Tool input (the branch's field values)
  }
  deriving stock (Eq, Show)

instance ToJSON ToolCall where
  toJSON t = object
    [ "name" .= t.tcName
    , "input" .= t.tcInput
    ]

instance FromJSON ToolCall where
  parseJSON = Aeson.withObject "ToolCall" $ \o -> do
    tcName <- o .: "name"
    tcInput <- o .: "input"
    pure ToolCall{..}


-- ════════════════════════════════════════════════════════════════════════════
-- TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Types that can be represented as MCP decision tools.
--
-- This is for sum types where each constructor becomes an MCP tool.
-- Claude Code calls one of these tools to "select" a branch.
--
-- @
-- data ReviewDecision = Approve {...} | Reject {...} | Escalate {...}
--   deriving (Generic)
--
-- instance ToDecisionTools ReviewDecision
--
-- tools = toDecisionTools @ReviewDecision
-- -- [DecisionTool "decision::approve" ..., DecisionTool "decision::reject" ...]
--
-- parsed = parseToolCall @ReviewDecision (ToolCall "decision::approve" {...})
-- -- Right (Approve {...})
-- @
class ToDecisionTools a where
  -- | Generate MCP tool definitions for each constructor.
  toDecisionTools :: [DecisionTool]

  -- | Parse a tool call back to the sum type.
  --
  -- Returns @Left@ with error message if the tool name doesn't match
  -- any constructor or the input fails to parse.
  parseToolCall :: ToolCall -> Either String a

  default toDecisionTools :: GDecisionTools (Rep a) => [DecisionTool]
  toDecisionTools = gToDecisionTools @(Rep a) defaultOptions

  default parseToolCall :: (Generic a, GDecisionTools (Rep a)) => ToolCall -> Either String a
  parseToolCall tc = fmap to $ gParseToolCall @(Rep a) defaultOptions tc


-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC MACHINERY
-- ════════════════════════════════════════════════════════════════════════════

-- | Generic class for deriving decision tools from sum types.
class GDecisionTools (f :: Type -> Type) where
  -- | Generate tool definitions.
  gToDecisionTools :: StructuredOptions -> [DecisionTool]

  -- | Try to parse a tool call.
  gParseToolCall :: StructuredOptions -> ToolCall -> Either String (f p)


-- | Pass through datatype metadata.
instance GDecisionTools f => GDecisionTools (M1 D d f) where
  gToDecisionTools opts = gToDecisionTools @f opts
  gParseToolCall opts tc = M1 <$> gParseToolCall @f opts tc


-- | Sum type: generate tools for each branch.
instance (GDecisionTools l, GDecisionTools r) => GDecisionTools (l :+: r) where
  gToDecisionTools opts =
    gToDecisionTools @l opts ++ gToDecisionTools @r opts

  gParseToolCall opts tc =
    case gParseToolCall @l opts tc of
      Right x -> Right (L1 x)
      Left _ -> case gParseToolCall @r opts tc of
        Right x -> Right (R1 x)
        Left e -> Left e  -- Return last error


-- | Single constructor: generate one tool.
instance (Constructor c, GStructuredProduct f) => GDecisionTools (M1 C c f) where
  gToDecisionTools opts =
    let rawName = conName (undefined :: M1 C c f p)
        toolName = decisionServerName <> "::" <> T.pack (camelToSnake rawName)
        description = "Select the " <> T.pack rawName <> " branch"

        -- Get field schema (reusing GStructuredProduct machinery)
        rawFieldNames = gProductRawFieldNames @f
        commonPfx = detectPrefix rawFieldNames
        prefixModifier = makeStripPrefix commonPfx
        opts' = opts { soFieldLabelModifier = prefixModifier }
        fields = gProductSchema @f opts'
        required = gProductRequired @f opts'
        schema = objectSchema (map (\(k, v) -> (T.pack k, v)) fields) (map T.pack required)
    in [DecisionTool
          { dtName = toolName
          , dtDescription = description
          , dtInputSchema = schemaToValue schema
          }]

  gParseToolCall opts tc =
    let rawName = conName (undefined :: M1 C c f p)
        expectedToolName = decisionServerName <> "::" <> T.pack (camelToSnake rawName)
    in if tc.tcName == expectedToolName
       then case tc.tcInput of
         Object obj -> do
           let rawFieldNames = gProductRawFieldNames @f
               commonPfx = detectPrefix rawFieldNames
               prefixModifier = makeStripPrefix commonPfx
               opts' = opts { soFieldLabelModifier = prefixModifier }
           M1 <$> first show (gProductParse @f opts' [] obj)
         _ -> Left $ "Expected object input for tool " <> T.unpack tc.tcName
       else Left $ "Tool name mismatch: expected " <> T.unpack expectedToolName
                   <> ", got " <> T.unpack tc.tcName


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert PascalCase to snake_case.
--
-- @camelToSnake "ApproveRequest" = "approve_request"@
camelToSnake :: String -> String
camelToSnake = go True
  where
    go _ [] = []
    go isFirst (c:cs)
      | isUpper c =
          let lower = toLower c
          in if isFirst
             then lower : go False cs
             else '_' : lower : go False cs
      | otherwise = c : go False cs
