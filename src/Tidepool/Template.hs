{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Template types for Tidepool game loops
module Tidepool.Template
  ( -- * Template Type
    Template(..)

    -- * Rendering
  , render

    -- * Schema
  , Schema(..)

    -- * Re-exports
  , ToolList(..)
  , TypedTemplate(..)
  , typedTemplateFile
  , runTypedTemplate
  ) where

import Data.Text (Text)
import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)
import GHC.Generics (Generic)
import Data.Aeson (Value)

import Tidepool.Tool (ToolList(..))

-- ══════════════════════════════════════════════════════════════
-- TYPED JINJA TEMPLATES (WIP - will be provided by external package)
-- ══════════════════════════════════════════════════════════════

-- | A compile-time validated Jinja template with type-safe context
-- The 'pos' parameter tracks source positions for error messages
data TypedTemplate ctx pos = TypedTemplate
  { templateCompiled :: ctx -> Text  -- compiled render function
  }

-- | Compile a Jinja template file at compile time, validating against context type
--
-- Usage:
-- @
-- dmTemplate :: TypedTemplate DMContext SourcePos
-- dmTemplate = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
-- @
typedTemplateFile :: Name -> FilePath -> Q Exp
typedTemplateFile _contextType _templatePath =
  error "TODO: typedTemplateFile - provided by jinja-th package"

-- | Render a typed template with a context value
runTypedTemplate :: ctx -> TypedTemplate ctx pos -> Text
runTypedTemplate ctx tmpl = tmpl.templateCompiled ctx

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATE (combines typed template + schema + tools)
-- ══════════════════════════════════════════════════════════════

-- | A Tidepool template combines:
-- - A typed Jinja template for rendering context to prompt
-- - A JSON Schema for structured output
-- - A type-safe list of available tools
--
-- Example:
-- @
-- dmTurnTemplate :: Template DMContext TurnOutput '[ThinkAsDM, SpeakAsNPC]
-- dmTurnTemplate = Template
--   { templateJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
--   , templateOutputSchema = turnOutputSchema
--   , templateTools = TCons (Proxy @ThinkAsDM)
--                   $ TCons (Proxy @SpeakAsNPC)
--                   $ TNil
--   }
-- @
data Template context output (tools :: [Type]) = Template
  { templateJinja :: TypedTemplate context ()  -- ^ Compiled Jinja template
  , templateOutputSchema :: Schema output      -- ^ JSON Schema for structured output
  , templateTools :: ToolList tools            -- ^ Type-safe list of available tools
  }

-- | JSON schema with descriptions for structured output
data Schema a = Schema
  { schemaJSON :: Value
  , schemaDescription :: Text
  }
  deriving (Show, Eq, Generic)

-- | Render a template with context
render :: Template context output tools -> context -> Text
render t ctx = runTypedTemplate ctx (t.templateJinja)
