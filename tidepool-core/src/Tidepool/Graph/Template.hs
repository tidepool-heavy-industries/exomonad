{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-safe template definitions for Graph nodes.
--
-- Templates are first-class entities that combine:
--
-- * A Jinja template file (validated at compile time via ginger TH)
-- * A context type that the template renders
-- * An effect-polymorphic context builder
--
-- = Design Philosophy
--
-- "Templates are typed. Context building is effectful. Validation is compile-time."
--
-- The ginger library's `typedTemplateFile` TH splice validates that all
-- @{{ variable }}@ references in the .jinja file correspond to fields in
-- the context type's `ToGVal` instance. This happens at compile time.
--
-- = Usage
--
-- Templates require a two-phase definition due to TH staging:
--
-- @
-- -- Phase 1: Compile the template (TH validates against context type)
-- classifyCompiled :: TypedTemplate ClassifyContext SourcePos
-- classifyCompiled = $(typedTemplateFile ''ClassifyContext "templates/classify.jinja")
--
-- -- Phase 2: Define the template type and instance
-- data ClassifyTpl
--
-- instance TemplateDef ClassifyTpl where
--   type TemplateContext ClassifyTpl = ClassifyContext
--   type TemplateConstraint ClassifyTpl es = (State SessionState :> es)
--
--   templateName = "classify"
--   templateDescription = "Classify user intent into categories"
--   templateCompiled = classifyCompiled
--
--   buildContext = do
--     st <- get \@SessionState
--     pure ClassifyContext
--       { ccMessages = st.messages
--       , ccCategories = st.availableCategories
--       }
-- @
--
-- = Rendering
--
-- Use 'renderTemplate' to build context and render in one step:
--
-- @
-- prompt <- renderTemplate \@ClassifyTpl
-- @
--
-- = Graph Integration
--
-- Templates integrate with the Graph DSL via the @Template@ annotation:
--
-- @
-- type MyGraph = Graph
--   '[ Entry :~> Message
--    , "classify" := LLM
--        :@ Needs '[Message]
--        :@ Template ClassifyTpl
--        :@ Schema Intent
--    , Exit :<~ Intent
--    ]
-- @
--
-- The graph runner will use 'renderTemplate' to generate prompts for LLM nodes.
module Tidepool.Graph.Template
  ( -- * Template Definition
    TemplateDef(..)

    -- * Rendering
  , renderTemplate

    -- * Ginger Re-exports (core)
  , TypedTemplate
  , typedTemplateFile
  , runTypedTemplate
  , GingerContext

    -- * Ginger Re-exports (dependency tracking)
  , TemplateDependency(..)
  , DepRelation(..)
  , DepLocation(..)
  , TemplateContextInfo(..)
  , templateDependencyTree
  , flattenDeps
  , templateContextInfo
  , templateAccessedFields

    -- * Template Haskell Helper
  , makeTemplateCompiled
  ) where

import Data.Char (toLower)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Control.Monad.Writer (Writer)
import Control.Monad.Freer (Eff)
import Language.Haskell.TH hiding (Type)
import Text.Ginger.GVal (ToGVal)
import Text.Ginger.Run.Type (Run)
import Text.Ginger.TH
  ( TypedTemplate
  , typedTemplateFile
  , runTypedTemplate
  , templateDependencyTree
  , flattenDeps
  , templateContextInfo
  , templateAccessedFields
  )
import Text.Ginger.TH.Types
  ( TemplateDependency(..)
  , DepRelation(..)
  , DepLocation(..)
  , TemplateContextInfo(..)
  )
import Text.Parsec.Pos (SourcePos)

-- ════════════════════════════════════════════════════════════════════════════
-- GINGER CONTEXT CONSTRAINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Constraint for types that can be rendered by ginger templates.
--
-- This is the specific monad stack that ginger uses internally.
type GingerContext ctx = ToGVal (Run SourcePos (Writer Text) Text) ctx

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DEFINITION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for template definitions.
--
-- Each template type (e.g., @ClassifyTpl@) is a phantom type that carries
-- all the template's configuration at the type level.
--
-- == Associated Types
--
-- * 'TemplateContext': The Haskell record type rendered into the template
-- * 'TemplateConstraint': Effects needed to build the context
--
-- == Methods
--
-- * 'templateName': Short identifier for logging/debugging
-- * 'templateDescription': Human-readable description
-- * 'templateCompiled': The ginger-compiled template (from TH splice)
-- * 'buildContext': Effectful computation to build the context
type TemplateDef :: Type -> Constraint
class TemplateDef t where
  -- | The context type that this template renders.
  --
  -- Must have a 'ToGVal' instance for ginger rendering.
  type TemplateContext t :: Type

  -- | Effects required to build the context.
  --
  -- Default is no effects (empty constraint).
  --
  -- @
  -- type TemplateConstraint MyTpl effs = (Member (State S) effs, Member Log effs)
  -- @
  type TemplateConstraint t (effs :: [Type -> Type]) :: Constraint
  type TemplateConstraint t effs = ()

  -- | Short name for this template (used in logging, debugging).
  templateName :: Text

  -- | Human-readable description of what this template does.
  templateDescription :: Text

  -- | The compiled ginger template.
  --
  -- This should reference a top-level binding created by 'typedTemplateFile':
  --
  -- @
  -- myCompiled :: TypedTemplate MyContext SourcePos
  -- myCompiled = $(typedTemplateFile ''MyContext "templates/my.jinja")
  --
  -- instance TemplateDef MyTpl where
  --   templateCompiled = myCompiled
  -- @
  templateCompiled :: TypedTemplate (TemplateContext t) SourcePos

  -- | Build the context from the current effect state.
  --
  -- This is where you gather data from State, Memory, etc. and
  -- construct the context record that gets rendered into the template.
  --
  -- @
  -- buildContext = do
  --   st <- get \@SessionState
  --   mem <- getMem \@NodeMemory
  --   pure MyContext { ... }
  -- @
  buildContext :: TemplateConstraint t effs => Eff effs (TemplateContext t)

  -- ════════════════════════════════════════════════════════════════════════════
  -- DERIVED ACCESSORS (for documentation generation)
  -- ════════════════════════════════════════════════════════════════════════════

  -- | Template dependency tree with include hierarchy.
  --
  -- Extracted from 'templateCompiled'. The root is the main template file,
  -- with 'depChildren' containing included/extended templates recursively.
  --
  -- Each node has:
  --
  -- * 'depAbsolutePath' / 'depRelativePath' - file paths
  -- * 'depRelation' - 'DepIncluded' or 'DepExtended' (Nothing for root)
  -- * 'depIncludeLocation' - line/col of the directive
  -- * 'depChildren' - nested includes/extends
  --
  -- Use 'Tidepool.Graph.Docs.renderDepTree' for text documentation.
  templateDepTree :: TemplateDependency
  templateDepTree = templateDependencyTree (templateCompiled @t)

  -- | Flattened list of all template dependencies (pre-order traversal).
  --
  -- Convenience accessor for iteration. Use 'templateDepTree' for hierarchy.
  templateDeps :: [TemplateDependency]
  templateDeps = flattenDeps (templateDepTree @t)

  -- | Fields accessed by the template (e.g., @["user.name", "user.email"]@).
  --
  -- Useful for documentation to show what context fields are actually used.
  templateFields :: [String]
  templateFields = templateAccessedFields (templateCompiled @t)

  -- | Context type metadata (type name, module, fully qualified name).
  --
  -- Useful for documentation generation.
  templateContextMeta :: TemplateContextInfo
  templateContextMeta = templateContextInfo (templateCompiled @t)

-- ════════════════════════════════════════════════════════════════════════════
-- RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Build context and render template in one step.
--
-- @
-- prompt <- renderTemplate \@ClassifyTpl
-- -- prompt :: Text (the rendered Jinja template)
-- @
--
-- This is the primary way to use templates. It:
--
-- 1. Calls 'buildContext' to gather data from effects
-- 2. Calls 'runTypedTemplate' to render the Jinja template
renderTemplate
  :: forall t effs.
     ( TemplateDef t
     , GingerContext (TemplateContext t)
     , TemplateConstraint t effs
     )
  => Eff effs Text
renderTemplate = do
  ctx <- buildContext @t
  pure $ runTypedTemplate ctx (templateCompiled @t)

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE HASKELL HELPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a compiled template binding.
--
-- This is a convenience helper that generates the top-level binding
-- for the compiled template:
--
-- @
-- $(makeTemplateCompiled ''MyContext "templates/my.jinja")
-- @
--
-- Generates:
--
-- @
-- myContext_compiled :: TypedTemplate MyContext SourcePos
-- myContext_compiled = $(typedTemplateFile ''MyContext "templates/my.jinja")
-- @
--
-- The binding name is derived from the context type name by appending
-- @_compiled@ and lowercasing the first letter.
makeTemplateCompiled :: Name -> FilePath -> Q [Dec]
makeTemplateCompiled ctxName path = do
  let baseName = nameBase ctxName
      bindingName = mkName $ lowerFirst baseName ++ "_compiled"

  -- Generate the type signature
  sigD <- sigD bindingName
    [t| TypedTemplate $(conT ctxName) SourcePos |]

  -- Generate the binding
  valD <- valD (varP bindingName)
    (normalB [| $(typedTemplateFile ctxName path) |])
    []

  pure [sigD, valD]
  where
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (c:cs) = toLower c : cs

    -- toLower is imported from Data.Char
