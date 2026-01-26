{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Documentation generation for Graph templates.
--
-- This module provides utilities for rendering template dependency trees
-- as text documentation. Use with 'TemplateDef' to generate per-node
-- documentation showing template file hierarchies.
--
-- = Example Output
--
-- @
-- templates/classify.jinja
--    ├─ partials/system.jinja
--    └─ partials/intent_examples.jinja
--         └─ partials/example_format.jinja
-- @
--
-- = Usage
--
-- @
-- import ExoMonad.Graph.Docs
-- import ExoMonad.Graph.Template
--
-- -- For a TemplateDef instance:
-- docs :: Text
-- docs = templateDocBlock \@ClassifyTpl
--
-- -- Or render the dependency tree directly:
-- let tree = templateDepTree \@ClassifyTpl
-- putStrLn (renderDepTree tree)
-- @
module ExoMonad.Graph.Docs
  ( -- * Tree Rendering
    renderDepTree
  , renderDepTreeCompact

    -- * Template Documentation
  , templateDocBlock
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Graph.Template
  ( TemplateDef(..)
  , TemplateDependency(..)
  , DepRelation(..)
  , TemplateContextInfo(..)
  )

-- ════════════════════════════════════════════════════════════════════════════
-- TREE RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a dependency tree as indented text with box-drawing characters.
--
-- Takes ginger's 'TemplateDependency' tree directly (via 'templateDepTree').
--
-- Output example:
--
-- @
-- templates/main.jinja
--    ├─ partials/header.jinja
--    │    └─ partials/nav.jinja
--    └─ partials/footer.jinja
-- @
renderDepTree :: TemplateDependency -> Text
renderDepTree = T.unlines . renderLines ""
  where
    renderLines :: Text -> TemplateDependency -> [Text]
    renderLines prefix dep =
      (prefix <> T.pack dep.depRelativePath <> relationSuffix dep)
      : concatMap (renderChild prefix) (withPosition $ sortedChildren dep)

    renderChild :: Text -> (TemplateDependency, Bool) -> [Text]
    renderChild prefix (child, isLast) =
      let connector = if isLast then "└─ " else "├─ "
          extension = if isLast then "   " else "│  "
          childPrefix = prefix <> "   "  -- Indent for children
      in case child.depChildren of
        [] -> [childPrefix <> connector <> T.pack child.depRelativePath <> relationSuffix child]
        _  -> (childPrefix <> connector <> T.pack child.depRelativePath <> relationSuffix child)
              : concatMap (renderChild (childPrefix <> extension)) (withPosition $ sortedChildren child)

    relationSuffix :: TemplateDependency -> Text
    relationSuffix dep = case dep.depRelation of
      Just DepExtended -> " (extends)"
      _ -> ""

    -- Sort children by relative path for consistent output
    sortedChildren :: TemplateDependency -> [TemplateDependency]
    sortedChildren dep = sortOn (.depRelativePath) dep.depChildren

    -- Tag each element with whether it's the last in the list
    withPosition :: [a] -> [(a, Bool)]
    withPosition [] = []
    withPosition xs = zip xs (replicate (length xs - 1) False ++ [True])

-- | Render a compact single-line representation.
--
-- Output example: @main.jinja -> header.jinja, footer.jinja@
renderDepTreeCompact :: TemplateDependency -> Text
renderDepTreeCompact dep = T.pack dep.depRelativePath <> renderChildren dep.depChildren
  where
    renderChildren [] = ""
    renderChildren cs = " -> " <> T.intercalate ", " (map (T.pack . (.depRelativePath)) cs)

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DOCUMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a documentation block for a 'TemplateDef'.
--
-- Includes:
--
-- * Template name and description
-- * Context type (fully qualified)
-- * Dependency tree
-- * Accessed fields
--
-- Output example:
--
-- @
-- ### classify
-- Classify user intent into categories
--
-- **Context**: MyModule.ClassifyContext
--
-- **Template**:
-- templates/classify.jinja
--    └─ partials/system.jinja
--
-- **Fields**: topic, categories
-- @
templateDocBlock :: forall t. TemplateDef t => Text
templateDocBlock = T.unlines $ filter (not . T.null)
  [ "### " <> templateName @t
  , templateDescription @t
  , ""
  , "**Context**: " <> T.pack (tciFullyQualified (templateContextMeta @t))
  , ""
  , "**Template**:\n" <> renderDepTree (templateDepTree @t)
  , if null (templateFields @t)
      then ""
      else "**Fields**: " <> T.intercalate ", " (map T.pack $ templateFields @t)
  ]
