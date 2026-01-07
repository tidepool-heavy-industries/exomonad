{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the types-first development workflow graph.
--
-- Sequential version: only the types node is active.
-- Future iterations will add fork/merge/testLoop handlers.
module TypesFirstDev.Handlers
  ( typesFirstHandlers

    -- * Effect Stack
  , DevEffects
  ) where

import Control.Monad.Freer (Eff)
import Data.Proxy (Proxy(..))

import Tidepool.Effect.ClaudeCode (ClaudeCodeExec)
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (GotoChoice, To, ClaudeCodeLLMHandler(..), gotoExit)
import Tidepool.Graph.Template (templateCompiled)
import Tidepool.Graph.Types (ModelChoice(..), Exit)

import TypesFirstDev.Context (TypesContext(..))
import TypesFirstDev.Graph (TypesFirstGraph(..))
import TypesFirstDev.Types (StackSpec(..), TypeDefinitions)
import TypesFirstDev.Templates (TypesTpl)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for the types-first workflow (sequential version).
--
-- - ClaudeCodeExec: For spawning Claude Code subprocesses
-- - IO: For system operations
type DevEffects = '[ClaudeCodeExec, IO]


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handlers for TypesFirstGraph (sequential version).
--
-- Only the types node is active - it writes type signatures and exits.
typesFirstHandlers :: TypesFirstGraph (AsHandler DevEffects)
typesFirstHandlers = TypesFirstGraph
  { entry = Proxy @StackSpec

    -- Types handler: Writes type signatures
  , types = ClaudeCodeLLMHandler @'Sonnet @'Nothing
      Nothing                              -- no system template
      (templateCompiled @TypesTpl)         -- user template
      buildTypesContext                    -- before: builds context
      routeAfterTypes                      -- after: exits with output

  , exit = Proxy @TypeDefinitions
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES NODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context for the types template.
buildTypesContext
  :: StackSpec
  -> Eff DevEffects TypesContext
buildTypesContext spec = pure TypesContext
  { moduleName = spec.ssModuleName
  , description = spec.ssDescription
  }

-- | Route after types agent completes.
--
-- Exits directly with the type definitions.
routeAfterTypes
  :: TypeDefinitions
  -> Eff DevEffects (GotoChoice '[To Exit TypeDefinitions])
routeAfterTypes = pure . gotoExit
