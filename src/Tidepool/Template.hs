{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Template types for Tidepool game loops
module Tidepool.Template
  ( -- * Template Type
    Template(..)

    -- * Rendering
  , render
  , GingerContext

    -- * Schema
  , Schema(..)

    -- * Re-exports from ginger
  , ToolList(..)
  , TypedTemplate
  , typedTemplateFile
  , runTypedTemplate
  ) where

import Data.Text (Text)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Aeson (Value)
import Text.Parsec.Pos (SourcePos)
import Effectful (Effect)

import Tidepool.Tool (ToolList(..))

-- Ginger imports
import Text.Ginger.TH (TypedTemplate, typedTemplateFile, runTypedTemplate)
import Text.Ginger.GVal (ToGVal)
import Text.Ginger.Run.Type (Run)
import Control.Monad.Writer (Writer)

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATE (combines typed template + schema + tools)
-- ══════════════════════════════════════════════════════════════

-- | A Tidepool template combines:
-- - A typed Jinja template for rendering context to prompt
-- - A JSON Schema for structured output
-- - A type-safe list of available tools
--
-- The event type parameter specifies what events the tools can emit.
--
-- Example:
-- @
-- dmTurnTemplate :: Template DMContext TurnOutput DMEvent '[ThinkAsDM, SpeakAsNPC]
-- dmTurnTemplate = Template
--   { templateJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
--   , templateOutputSchema = turnOutputSchema
--   , templateTools = TCons (Proxy @ThinkAsDM)
--                   $ TCons (Proxy @SpeakAsNPC)
--                   $ TNil
--   }
-- @
data Template context output event state (extraEs :: [Effect]) (tools :: [Type]) = Template
  { templateJinja :: TypedTemplate context SourcePos  -- ^ Compiled Jinja template
  , templateOutputSchema :: Schema output             -- ^ JSON Schema for structured output
  , templateTools :: ToolList event state extraEs tools  -- ^ Type-safe list of available tools
  }

-- | JSON schema with descriptions for structured output
data Schema a = Schema
  { schemaJSON :: Value
  , schemaDescription :: Text
  }
  deriving (Show, Eq, Generic)

-- | Constraint for types that can be rendered by ginger templates
type GingerContext ctx = ToGVal (Run SourcePos (Writer Text) Text) ctx

-- | Render a template with context
-- Note: context type must have ToGVal instance for ginger
render :: GingerContext context => Template context output event state extraEs tools -> context -> Text
render t ctx = runTypedTemplate ctx (t.templateJinja)
