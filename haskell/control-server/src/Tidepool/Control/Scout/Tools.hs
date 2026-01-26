{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ToolDef wrapper for ScoutGemma effect
--
-- This bridges the ScoutGemma effect to the ToolDef interface,
-- providing schema/metadata for FunctionGemma prompts.
module Tidepool.Control.Scout.Tools
  ( SelectSymbolsTool(..)
  , SelectSymbolsInput(..)
  , SelectSymbolsOutput(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)

import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (ToAnthropicTool, ToCfTool)
import Tidepool.Schema (HasJSONSchema(..))
import Tidepool.Control.Scout.DocGen.Gemma (ScoutGemma, selectRelevantSymbols)
import Tidepool.Control.Scout.DocGen.Types (LSPSymbol(..))
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
    -- This tool is executed via ScoutGemma effect, not via toolExecute.
    -- The ToolDef instance exists only to provide schema/metadata for FunctionGemma.
    error "SelectSymbolsTool.toolExecute should never be called - use runScoutGemmaHTTP"

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
parseLocation text =
  let parts = T.splitOn ":" text
      n = length parts
      defaultPos = Position { posLine = 0, posCharacter = 0 }
      defaultRange = Range { rangeStart = defaultPos, rangeEnd = defaultPos }
      defaultLoc = Location { locUri = "file:///unknown", locRange = defaultRange }
      
      -- Helper to parse int from text
      parseInt t = case TR.decimal t of
        Right (i, _) -> i
        Left _ -> 0
  in
  if n >= 3 then
    let colStr = last parts
        lineStr = last (init parts)
        uriParts = init (init parts)
        uri = T.intercalate ":" uriParts
        
        line = parseInt lineStr
        col = parseInt colStr
        
        pos = Position { posLine = line, posCharacter = col }
        range = Range { rangeStart = pos, rangeEnd = pos }
    in Location { locUri = uri, locRange = range }
  else
    defaultLoc
