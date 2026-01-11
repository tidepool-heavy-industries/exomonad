{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Schema handling for ClaudeCode nodes.
--
-- This module provides type-level dispatch for ClaudeCode node outputs:
--
-- * Sum types with data → Generate MCP decision tools
-- * Everything else → Use standard structured output
--
-- = Architecture
--
-- @
-- data ReviewDecision
--   = Approve { notes :: Text }
--   | Reject { reason :: Text }
--   deriving (Generic, ToDecisionTools)
--
-- -- Sum type with decision tools:
-- instance ClaudeCodeSchema ReviewDecision where
--   ccDecisionTools = Just (toDecisionTools @ReviewDecision)
--   ccParseToolCall = parseToolCall @ReviewDecision
--
-- -- For regular types, just derive:
-- data Output = Output { result :: Text }
--   deriving Generic
-- instance StructuredOutput Output
-- instance ClaudeCodeSchema Output  -- Uses defaults (no tools)
-- @
module Tidepool.StructuredOutput.ClaudeCodeSchema
  ( -- * Typeclass
    ClaudeCodeSchema(..)

    -- * Type-Level Detection
  , IsSumWithData
  ) where

import Data.Aeson (Value)
import Data.Kind (Type)

import Tidepool.Schema (HasSumRep, IsNullarySum)
import Tidepool.StructuredOutput.Class (StructuredOutput(..))
import Tidepool.StructuredOutput.DecisionTools (DecisionTool)
import qualified Tidepool.StructuredOutput.DecisionTools as DT
import Tidepool.StructuredOutput.Error (ParseDiagnostic)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Detect if a type is a sum type with data (not a nullary enum).
--
-- Returns 'True for sum types like @data Choice = A Text | B Int@
-- Returns 'False for:
-- * Product types (records)
-- * Nullary enums like @data Priority = Low | High@
-- * Primitive types
type family IsSumWithData (t :: Type) :: Bool where
  IsSumWithData t = IsSumWithDataImpl (HasSumRep t) (IsNullarySum t)

type family IsSumWithDataImpl (hasSum :: Bool) (isNullary :: Bool) :: Bool where
  IsSumWithDataImpl 'True 'False = 'True   -- Sum with data
  IsSumWithDataImpl _ _ = 'False           -- Not a sum, or nullary enum


-- ════════════════════════════════════════════════════════════════════════════
-- TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for types used as ClaudeCode node outputs.
--
-- This provides:
-- * Decision tools for sum types with data
-- * Parsing from either tool calls or structured output
--
-- For sum types with data, Claude Code is given MCP tools and should call
-- one to indicate its choice. For other types, standard structured output
-- is used.
--
-- = Default Behavior
--
-- By default (empty instance), a type uses structured output:
--
-- @
-- data Output = Output { result :: Text }
--   deriving Generic
-- instance StructuredOutput Output
-- instance ClaudeCodeSchema Output  -- Uses defaults
-- @
--
-- = Sum Types with Decision Tools
--
-- For sum types that should use decision tools, override the methods:
--
-- @
-- data Decision = Approve Text | Reject Text
--   deriving (Generic, ToDecisionTools)
--
-- instance ClaudeCodeSchema Decision where
--   ccDecisionTools = Just (toDecisionTools @Decision)
--   ccParseToolCall = parseToolCall @Decision
-- @
class StructuredOutput a => ClaudeCodeSchema a where
  -- | Get decision tools if this is a sum type with data.
  --
  -- Returns 'Just' with tool definitions for sum types.
  -- Returns 'Nothing' for product types and nullary enums.
  --
  -- Default: 'Nothing' (use structured output)
  ccDecisionTools :: Maybe [DecisionTool]
  ccDecisionTools = Nothing

  -- | Parse from Claude Code tool call.
  --
  -- For sum types, parses the tool call to construct the value.
  -- For other types, returns 'Left' (tool calls not expected).
  --
  -- Default: Returns error (tool calls not expected)
  ccParseToolCall :: DT.ToolCall -> Either String a
  ccParseToolCall tc = Left $ "Unexpected tool call: " <> show tc.tcName
    <> " (this type does not use decision tools)"

  -- | Parse from structured output JSON.
  --
  -- Uses standard StructuredOutput parsing.
  --
  -- Default: Delegates to 'parseStructured'
  ccParseStructured :: Value -> Either ParseDiagnostic a
  ccParseStructured = parseStructured
