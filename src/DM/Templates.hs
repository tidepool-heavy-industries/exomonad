-- | DM Templates - compile-time type-checked Jinja templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate

    -- * Typed Jinja Templates
  , dmTurnJinja
  , compressionJinja

    -- * Rendering
  , renderDMTurn
  , renderCompression
  ) where

import DM.Context
import DM.Output
import DM.Tools
import Tidepool.Template
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

-- ══════════════════════════════════════════════════════════════
-- TYPED JINJA TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | DM turn Jinja template (compile-time validated)
dmTurnJinja :: TypedTemplate DMContext SourcePos
dmTurnJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")

-- | Compression Jinja template (compile-time validated)
compressionJinja :: TypedTemplate CompressionContext SourcePos
compressionJinja = $(typedTemplateFile ''CompressionContext "templates/compression.jinja")

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | Main DM turn template with all available tools
dmTurnTemplate :: Template DMContext TurnOutput DMEvent '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = TCons (Proxy @ThinkAsDM)
                  $ TCons (Proxy @SpeakAsNPC)
                  $ TCons (Proxy @AskPlayer)
                  $ TCons (Proxy @Choose)
                  $ TNil
  }

-- | Compression template (no tools needed - uses unit event type)
compressionTemplate :: Template CompressionContext CompressionOutput () '[]
compressionTemplate = Template
  { templateJinja = compressionJinja
  , templateOutputSchema = compressionOutputSchema
  , templateTools = TNil
  }

-- ══════════════════════════════════════════════════════════════
-- RENDERING
-- ══════════════════════════════════════════════════════════════

-- | Render DM turn template with context
renderDMTurn :: DMContext -> Text
renderDMTurn = render dmTurnTemplate

-- | Render compression template with context
renderCompression :: CompressionContext -> Text
renderCompression = render compressionTemplate

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

turnOutputSchema :: Schema TurnOutput
turnOutputSchema = error "TODO: turnOutputSchema - build JSON Schema with field descriptions for TurnOutput"

compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = error "TODO: compressionOutputSchema - build JSON Schema for CompressionOutput"
