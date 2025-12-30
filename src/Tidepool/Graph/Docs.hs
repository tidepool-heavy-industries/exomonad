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
-- import Tidepool.Graph.Docs
-- import Tidepool.Graph.Template
--
-- -- For a TemplateDef instance:
-- docs :: Text
-- docs = templateDocBlock \@ClassifyTpl
--
-- -- Or build tree manually from dependencies:
-- let deps = templateDeps \@ClassifyTpl
-- case buildDepTree deps of
--   Just tree -> putStrLn (renderDepTree tree)
--   Nothing   -> putStrLn "No dependencies"
-- @
module Tidepool.Graph.Docs
  ( -- * Dependency Tree
    DepTree(..)
  , buildDepTree

    -- * Tree Rendering
  , renderDepTree
  , renderDepTreeCompact

    -- * Template Documentation
  , templateDocBlock
  ) where

import Data.List (partition, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Graph.Template
  ( TemplateDef(..)
  , TemplateDependency(..)
  , DepRelation(..)
  , TemplateContextInfo(..)
  )

-- ════════════════════════════════════════════════════════════════════════════
-- DEPENDENCY TREE
-- ════════════════════════════════════════════════════════════════════════════

-- | A tree of template dependencies.
--
-- Built from the flat list of 'TemplateDependency' using 'depIncludedBy'
-- to reconstruct the include hierarchy.
data DepTree = DepTree
  { dtPath :: FilePath           -- ^ Relative path to template file
  , dtRelation :: Maybe DepRelation  -- ^ How included (Nothing for root)
  , dtChildren :: [DepTree]      -- ^ Included/extended templates
  }
  deriving (Show, Eq)

-- | Build a dependency tree from a flat list.
--
-- Uses 'depIncludedBy' to reconstruct the hierarchy. Returns 'Nothing'
-- if the list is empty or has no root (no entry with 'depIncludedBy' = Nothing).
--
-- The tree is sorted by relative path at each level for consistent output.
buildDepTree :: [TemplateDependency] -> Maybe DepTree
buildDepTree deps = case roots of
  [] -> Nothing
  (root:_) -> Just $ buildNode root
  where
    -- Root is the dependency with no parent
    (roots, nonRoots) = partition (null . depIncludedBy) deps

    -- Build a node and its children recursively
    buildNode :: TemplateDependency -> DepTree
    buildNode dep = DepTree
      { dtPath = dep.depRelativePath
      , dtRelation = dep.depRelation
      , dtChildren = sortOn (.dtPath) $ map buildNode (childrenOf dep)
      }

    -- Find all deps that have this dep as their parent
    childrenOf :: TemplateDependency -> [TemplateDependency]
    childrenOf parent = filter isChildOf nonRoots
      where
        parentPath = parent.depAbsolutePath
        isChildOf child = child.depIncludedBy == Just parentPath

-- ════════════════════════════════════════════════════════════════════════════
-- TREE RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a dependency tree as indented text with box-drawing characters.
--
-- Output example:
--
-- @
-- templates/main.jinja
--    ├─ partials/header.jinja
--    │    └─ partials/nav.jinja
--    └─ partials/footer.jinja
-- @
renderDepTree :: DepTree -> Text
renderDepTree = T.unlines . renderLines ""
  where
    renderLines :: Text -> DepTree -> [Text]
    renderLines prefix tree =
      (prefix <> T.pack tree.dtPath <> relationSuffix tree)
      : concatMap (renderChild prefix) (withPosition tree.dtChildren)

    renderChild :: Text -> (DepTree, Bool) -> [Text]
    renderChild prefix (child, isLast) =
      let connector = if isLast then "└─ " else "├─ "
          extension = if isLast then "   " else "│  "
          childPrefix = prefix <> "   "  -- Indent for children
      in case child.dtChildren of
        [] -> [childPrefix <> connector <> T.pack child.dtPath <> relationSuffix child]
        _  -> (childPrefix <> connector <> T.pack child.dtPath <> relationSuffix child)
              : concatMap (renderChild (childPrefix <> extension)) (withPosition child.dtChildren)

    relationSuffix :: DepTree -> Text
    relationSuffix tree = case tree.dtRelation of
      Just DepExtended -> " (extends)"
      _ -> ""

    -- Tag each element with whether it's the last in the list
    withPosition :: [a] -> [(a, Bool)]
    withPosition [] = []
    withPosition xs = zip xs (replicate (length xs - 1) False ++ [True])

-- | Render a compact single-line representation.
--
-- Output example: @main.jinja -> header.jinja, footer.jinja@
renderDepTreeCompact :: DepTree -> Text
renderDepTreeCompact tree = T.pack tree.dtPath <> renderChildren tree.dtChildren
  where
    renderChildren [] = ""
    renderChildren cs = " -> " <> T.intercalate ", " (map (T.pack . (.dtPath)) cs)

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
  , case buildDepTree (templateDeps @t) of
      Just tree -> "**Template**:\n" <> renderDepTree tree
      Nothing   -> "**Template**: (no dependencies)"
  , if null (templateFields @t)
      then ""
      else "**Fields**: " <> T.intercalate ", " (map T.pack $ templateFields @t)
  ]
