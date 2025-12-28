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

    -- * ToGVal instances for common containers
    -- (HashMap, Seq, Either are not provided by ginger)
  ) where

import Data.Text (Text)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq)
import qualified Data.Foldable as F
import Text.Parsec.Pos (SourcePos)

import Tidepool.Tool (ToolList(..))

-- Ginger imports
import Text.Ginger.TH (TypedTemplate, typedTemplateFile, runTypedTemplate)
import Text.Ginger.GVal (ToGVal(..), list, dict)
import Text.Ginger.Run.Type (Run)
import Control.Monad.Writer (Writer)

-- ══════════════════════════════════════════════════════════════
-- TOGVAL CONTAINER INSTANCES (not provided by ginger)
-- ══════════════════════════════════════════════════════════════

instance (ToGVal m k, ToGVal m v) => ToGVal m (HashMap k v) where
  toGVal hm = list [toGVal (k, v) | (k, v) <- HM.toList hm]

instance ToGVal m a => ToGVal m (Seq a) where
  toGVal s = list (map toGVal (F.toList s))

instance (ToGVal m a, ToGVal m b) => ToGVal m (Either a b) where
  toGVal (Left a) = dict [("Left", toGVal a)]
  toGVal (Right b) = dict [("Right", toGVal b)]

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
data Template context output event state (tools :: [Type]) = Template
  { templateJinja :: TypedTemplate context SourcePos  -- ^ Compiled Jinja template
  , templateOutputSchema :: Schema output             -- ^ JSON Schema for structured output
  , templateTools :: ToolList event state tools       -- ^ Type-safe list of available tools
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
render :: GingerContext context => Template context output event state tools -> context -> Text
render t ctx = runTypedTemplate ctx (t.templateJinja)
