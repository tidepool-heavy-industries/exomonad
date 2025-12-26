-- | DM Templates
--
-- When jinja-th is ready, replace the placeholder templates with:
-- @
-- dmTurnJinja :: TypedTemplate DMContext ()
-- dmTurnJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
-- @
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate

    -- * Typed Jinja Templates (placeholders until jinja-th ready)
  , dmTurnJinja
  , compressionJinja
  ) where

import DM.State (Clock, Thread, Rumor)
import DM.Context
import DM.Output
import DM.Tools
import Tidepool.Template
import Data.Proxy (Proxy(..))
import Data.Text (Text)

-- ══════════════════════════════════════════════════════════════
-- TYPED JINJA TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | DM turn Jinja template
-- TODO: Replace with: $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")
dmTurnJinja :: TypedTemplate DMContext ()
dmTurnJinja = TypedTemplate
  { templateCompiled = \_ctx -> error "TODO: replace with $(typedTemplateFile ''DMContext \"templates/dm_turn.jinja\")"
  }

-- | Compression Jinja template
-- TODO: Replace with: $(typedTemplateFile ''CompressionContext "templates/compression.jinja")
compressionJinja :: TypedTemplate CompressionContext ()
compressionJinja = TypedTemplate
  { templateCompiled = \_ctx -> error "TODO: replace with $(typedTemplateFile ''CompressionContext \"templates/compression.jinja\")"
  }

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | Main DM turn template with all available tools
dmTurnTemplate :: Template DMContext TurnOutput '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = TCons (Proxy @ThinkAsDM)
                  $ TCons (Proxy @SpeakAsNPC)
                  $ TCons (Proxy @AskPlayer)
                  $ TCons (Proxy @Choose)
                  $ TNil
  }

-- | Compression template (no tools needed)
compressionTemplate :: Template CompressionContext CompressionOutput '[]
compressionTemplate = Template
  { templateJinja = compressionJinja
  , templateOutputSchema = compressionOutputSchema
  , templateTools = TNil
  }

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

turnOutputSchema :: Schema TurnOutput
turnOutputSchema = error "TODO: turnOutputSchema - build JSON Schema with field descriptions for TurnOutput"

compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = error "TODO: compressionOutputSchema - build JSON Schema for CompressionOutput"
