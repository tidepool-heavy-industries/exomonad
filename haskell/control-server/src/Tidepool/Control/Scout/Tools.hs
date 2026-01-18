{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ToolDef wrapper for ScoutGemma effect
--
-- This bridges the ScoutGemma effect to the ToolDef interface,
-- enabling teaching mode via executeWithTeaching.
module Tidepool.Control.Scout.Tools
  ( SelectSymbolsTool(..)
  , SelectSymbolsInput(..)
  , SelectSymbolsOutput(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (ToAnthropicTool, ToCfTool)
import Tidepool.Schema (HasJSONSchema(..))
import Tidepool.Control.Scout.Teach.Gemma (ScoutGemma, selectRelevantSymbols)
import Tidepool.Control.Scout.Teach.Types (LSPSymbol(..))
import Tidepool.Effect.LSP (SymbolKind(..), Location(..), Position(..), Range(..))

-- | Tool marker for SelectRelevantSymbols operation
data SelectSymbolsTool = SelectSymbolsTool
  deriving (Show, Eq)

-- | Flattened input from ScoutGemma effect arguments
--
-- Combines:
-- - topic: what we're trying to understand
-- - symbol fields: flattened from LSPSymbol
-- - candidates: symbols extracted from signature
data SelectSymbolsInput = SelectSymbolsInput
  { topic :: Text
    -- ^ Topic description (e.g., "the scoring system")
  , symbolName :: Text
    -- ^ Symbol being analyzed
  , symbolKind :: Text
    -- ^ Symbol kind as text (Function, Class, etc.)
  , symbolLocation :: Text
    -- ^ Location as URI:line:col (flattened for JSON)
  , symbolSignature :: Text
    -- ^ Type signature from LSP hover
  , symbolDocComment :: Maybe Text
    -- ^ Documentation comment (if available)
  , candidates :: [Text]
    -- ^ Candidate symbols (pre-extracted from signature)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- | Tool output (selected subset of candidates)
newtype SelectSymbolsOutput = SelectSymbolsOutput
  { selected :: [Text]
    -- ^ Symbols relevant to the topic
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, HasJSONSchema)

-- | ToolDef instance bridges to ScoutGemma effect
instance ToolDef SelectSymbolsTool where
  type ToolInput SelectSymbolsTool = SelectSymbolsInput
  type ToolOutput SelectSymbolsTool = SelectSymbolsOutput
  type ToolEffects SelectSymbolsTool = '[IO]

  toolName _ = "select_symbols"

  toolDescription _ = "Select relevant symbols from candidates for understanding a topic. \
                      \Returns a subset of candidate symbols that are semantically relevant \
                      \to the specified topic, based on the context symbol's signature."

  toolExecute _ _ =
    -- This tool is executed via executeWithTeaching (Haiku API), not via toolExecute.
    -- The ToolDef instance exists only to provide schema/metadata for Haiku.
    error "SelectSymbolsTool.toolExecute should never be called - use runScoutGemmaWithTeaching or runScoutGemmaHTTP"

-- Auto-derive Anthropic and Cloudflare tool conversions
instance ToAnthropicTool SelectSymbolsTool
instance ToCfTool SelectSymbolsTool

-- ════════════════════════════════════════════════════════════════════════════
-- PARSING HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse SymbolKind from text representation
--
-- This is best-effort - defaults to SKFunction if unrecognized.
parseSymbolKind :: Text -> SymbolKind
parseSymbolKind "File" = SKFile
parseSymbolKind "Module" = SKModule
parseSymbolKind "Namespace" = SKNamespace
parseSymbolKind "Package" = SKPackage
parseSymbolKind "Class" = SKClass
parseSymbolKind "Method" = SKMethod
parseSymbolKind "Property" = SKProperty
parseSymbolKind "Field" = SKField
parseSymbolKind "Constructor" = SKConstructor
parseSymbolKind "Enum" = SKEnum
parseSymbolKind "Interface" = SKInterface
parseSymbolKind "Function" = SKFunction
parseSymbolKind "Variable" = SKVariable
parseSymbolKind "Constant" = SKConstant
parseSymbolKind "String" = SKString
parseSymbolKind "Number" = SKNumber
parseSymbolKind "Boolean" = SKBoolean
parseSymbolKind "Array" = SKArray
parseSymbolKind "Object" = SKObject
parseSymbolKind "Key" = SKKey
parseSymbolKind "Null" = SKNull
parseSymbolKind "EnumMember" = SKEnumMember
parseSymbolKind "Struct" = SKStruct
parseSymbolKind "Event" = SKEvent
parseSymbolKind "Operator" = SKOperator
parseSymbolKind "TypeParameter" = SKTypeParameter
parseSymbolKind _ = SKFunction  -- Default to function for unknown kinds

-- | Parse Location from "uri:line:col" format
--
-- Example: "file:///path/to/File.hs:42:10"
-- If parsing fails, returns a default location.
parseLocation :: Text -> Location
parseLocation _ =
  -- FIXME: This is a stub. Proper implementation would:
  -- 1. Split on ':' to extract URI, line, col
  -- 2. Parse line/col as Ints
  -- 3. Construct Location { locUri, locRange }
  -- For now, we use a placeholder since Location isn't critical for selection.
  let defaultPos = Position { posLine = 0, posCharacter = 0 }
      defaultRange = Range { rangeStart = defaultPos, rangeEnd = defaultPos }
  in Location
       { locUri = "file:///unknown"
       , locRange = defaultRange
       }
