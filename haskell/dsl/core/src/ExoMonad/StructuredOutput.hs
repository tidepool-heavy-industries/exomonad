-- | Unified structured output for LLM interactions.
--
-- This module provides the 'StructuredOutput' typeclass which derives
-- JSON Schema, encoding, and parsing from a single Generic traversal.
--
-- = Quick Start
--
-- For types with GHC.Generics, you need only an empty instance:
--
-- @
-- import GHC.Generics (Generic)
-- import ExoMonad.StructuredOutput
--
-- data TypeDefinitions = TypeDefinitions
--   { tdTypeName :: Text
--   , tdDataType :: Text
--   }
--   deriving stock (Generic)
--
-- instance StructuredOutput TypeDefinitions
-- @
--
-- That's it! The framework derives:
--
-- - 'structuredSchema' - JSON Schema for LLM constraints
-- - 'encodeStructured' - Serialize to JSON
-- - 'parseStructured' - Parse with detailed error diagnostics
--
-- = Field Name Transformation
--
-- By default, field names have their common lowercase prefix stripped:
--
-- @
-- data Example = Example { exName :: Text, exAge :: Int }
-- -- JSON keys: \"name\", \"age\" (prefix \"ex\" stripped)
-- @
--
-- = Better Error Messages
--
-- Parse errors include the exact field path:
--
-- @
-- Parse error at: signatures.0.name
-- Expected: string
-- Got: number: 42
-- @
--
-- = Custom Options
--
-- Override 'structuredOptions' for custom behavior:
--
-- @
-- instance StructuredOutput MyType where
--   structuredOptions = defaultOptions
--     { soFieldLabelModifier = camelTo2 '_'  -- Use snake_case
--     }
-- @
module ExoMonad.StructuredOutput
  ( -- * The Typeclass
    StructuredOutput (..),

    -- * Options
    StructuredOptions (..),
    SumEncoding (..),
    defaultOptions,

    -- * Error Types
    ParseDiagnostic (..),
    formatDiagnostic,

    -- * String Enum Wrapper
    StringEnum (..),
    ExoMonadDefault (..),

    -- * Field Label Utilities
    stripFieldPrefix,
    defaultFieldLabel,

    -- * ClaudeCode Schema Handling
    ClaudeCodeSchema (..),
    IsSumWithData,

    -- * Validation
    ValidStructuredOutput,
    ValidInContext,
    SchemaContext (..),

    -- * Decision Tools
    ToDecisionTools (..),
    DecisionTool (..),
    ToolCall (..),
  )
where

-- Import instances to bring them into scope
-- GStructuredOutput instances for M1, :*:, etc.
-- Base type instances

-- Re-export from submodules
import ExoMonad.StructuredOutput.Class
  ( ExoMonadDefault (..),
    SchemaContext (..),
    StringEnum (..),
    StructuredOptions (..),
    StructuredOutput (..),
    SumEncoding (..),
    ValidInContext,
    ValidStructuredOutput,
    defaultOptions,
  )
import ExoMonad.StructuredOutput.ClaudeCodeSchema
  ( ClaudeCodeSchema (..),
    IsSumWithData,
  )
import ExoMonad.StructuredOutput.DecisionTools
  ( DecisionTool (..),
    ToDecisionTools (..),
    ToolCall (..),
  )
import ExoMonad.StructuredOutput.Error
  ( ParseDiagnostic (..),
    formatDiagnostic,
  )
import ExoMonad.StructuredOutput.Generic ()
import ExoMonad.StructuredOutput.Instances ()
import ExoMonad.StructuredOutput.Prefix
  ( defaultFieldLabel,
    stripFieldPrefix,
  )

-- Note: Generic.hs instances are imported above to bring them into scope.
-- Users don't need to import it directly.
