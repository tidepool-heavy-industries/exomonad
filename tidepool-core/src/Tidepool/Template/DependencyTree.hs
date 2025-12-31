{-# LANGUAGE OverloadedStrings #-}

-- | Template dependency tree analysis and Mermaid generation.
--
-- This module provides functions to parse Jinja templates for dependency
-- directives ({% include %} and {% extends %}) and generate Mermaid diagrams
-- showing the template inclusion hierarchy.
--
-- = Usage
--
-- @
-- -- Generate Mermaid diagram from a template directory
-- diagram <- templateTreeToMermaid "templates/scene/main.jinja"
-- putStrLn diagram
-- @
--
-- = Example Output
--
-- @
-- flowchart TD
--     scene/main.jinja --> _shared/world_context.jinja
--     scene/main.jinja --> _shared/output_format.jinja
-- @
module Tidepool.Template.DependencyTree
  ( -- * Mermaid Generation
    templateTreeToMermaid
  , templateTreeToMermaidWithConfig
  , TemplateTreeConfig(..)
  , defaultTemplateTreeConfig

    -- * Template Parsing
  , parseTemplateIncludes
  , parseTemplateExtends
  , parseAllDependencies

    -- * Dependency Tree Building
  , buildDependencyTree
  , DependencyTree(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>), normalise)
import Text.Parsec
import Text.Parsec.Text (Parser)

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for template tree Mermaid generation.
data TemplateTreeConfig = TemplateTreeConfig
  { ttcDirection :: Text          -- ^ Flow direction: TD (top-down) or LR (left-right)
  , ttcShowPath :: Bool           -- ^ Show full paths or just filenames
  , ttcIncludeStyle :: Text       -- ^ Edge style for includes (e.g., "-->")
  , ttcExtendsStyle :: Text       -- ^ Edge style for extends (e.g., "-.->")
  }
  deriving (Show, Eq)

-- | Default configuration for template tree diagrams.
defaultTemplateTreeConfig :: TemplateTreeConfig
defaultTemplateTreeConfig = TemplateTreeConfig
  { ttcDirection = "TD"
  , ttcShowPath = True
  , ttcIncludeStyle = "-->"
  , ttcExtendsStyle = "-.->"
  }

-- ════════════════════════════════════════════════════════════════════════════
-- DEPENDENCY TREE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | A dependency tree for templates.
data DependencyTree = DependencyTree
  { dtRoot :: FilePath                    -- ^ Root template file
  , dtIncludes :: Map FilePath [FilePath] -- ^ Template -> list of included templates
  , dtExtends :: Map FilePath [FilePath]  -- ^ Template -> list of extended templates
  }
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- MERMAID GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a Mermaid diagram from a template file.
--
-- Parses the template and all transitively included/extended templates,
-- then generates a Mermaid flowchart showing the dependency structure.
templateTreeToMermaid :: FilePath -> IO Text
templateTreeToMermaid = templateTreeToMermaidWithConfig defaultTemplateTreeConfig

-- | Generate a Mermaid diagram with custom configuration.
templateTreeToMermaidWithConfig :: TemplateTreeConfig -> FilePath -> IO Text
templateTreeToMermaidWithConfig config rootPath = do
  tree <- buildDependencyTree rootPath
  pure $ renderDependencyTree config tree

-- | Render a dependency tree to Mermaid syntax.
renderDependencyTree :: TemplateTreeConfig -> DependencyTree -> Text
renderDependencyTree config tree = T.unlines $
  [ "flowchart " <> config.ttcDirection
  , ""
  , "    %% Template Dependencies"
  ]
  ++ includeEdges
  ++ extendsEdges
  where
    includeEdges =
      [ "    " <> escapeName (displayPath config from)
        <> " " <> config.ttcIncludeStyle <> " "
        <> escapeName (displayPath config to)
      | (from, tos) <- Map.toList (tree.dtIncludes)
      , to <- tos
      ]

    extendsEdges =
      [ "    " <> escapeName (displayPath config from)
        <> " " <> config.ttcExtendsStyle <> "|extends| "
        <> escapeName (displayPath config to)
      | (from, tos) <- Map.toList (tree.dtExtends)
      , to <- tos
      ]

-- | Display path according to config.
displayPath :: TemplateTreeConfig -> FilePath -> Text
displayPath config path
  | config.ttcShowPath = T.pack path
  | otherwise = T.pack $ takeFileName path
  where
    takeFileName = reverse . takeWhile (/= '/') . reverse

-- | Escape a node name for Mermaid.
escapeName :: Text -> Text
escapeName name
  | T.any needsEscape name = "\"" <> T.replace "\"" "\\\"" name <> "\""
  | otherwise = name
  where
    needsEscape c = c `elem` (" -()[]{}<>\"'/." :: String)

-- ════════════════════════════════════════════════════════════════════════════
-- DEPENDENCY TREE BUILDING
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a dependency tree starting from a root template.
--
-- Recursively parses all included and extended templates to build
-- a complete dependency graph.
buildDependencyTree :: FilePath -> IO DependencyTree
buildDependencyTree rootPath = do
  (includes, extends) <- buildTreeFrom rootPath Set.empty Map.empty Map.empty
  pure DependencyTree
    { dtRoot = rootPath
    , dtIncludes = includes
    , dtExtends = extends
    }

-- | Recursively build dependency maps.
buildTreeFrom
  :: FilePath
  -> Set FilePath           -- ^ Already visited paths (to avoid cycles)
  -> Map FilePath [FilePath] -- ^ Accumulated includes
  -> Map FilePath [FilePath] -- ^ Accumulated extends
  -> IO (Map FilePath [FilePath], Map FilePath [FilePath])
buildTreeFrom path visited incMap extMap
  | path `Set.member` visited = pure (incMap, extMap)  -- Cycle detected
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then pure (incMap, extMap)  -- File doesn't exist, skip
        else do
          content <- TIO.readFile path
          let baseDir = takeDirectory path
          let (incs, exts) = parseAllDependencies content

          -- Resolve relative paths
          let resolvedIncs = map (normalise . (baseDir </>)) incs
          let resolvedExts = map (normalise . (baseDir </>)) exts

          -- Update maps
          let incMap' = if null resolvedIncs then incMap else Map.insert path resolvedIncs incMap
          let extMap' = if null resolvedExts then extMap else Map.insert path resolvedExts extMap

          -- Recursively process dependencies
          let visited' = Set.insert path visited
          let allDeps = resolvedIncs ++ resolvedExts

          foldlM
            (\(im, em) dep -> buildTreeFrom dep visited' im em)
            (incMap', extMap')
            allDeps
  where
    foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
    foldlM _ acc [] = pure acc
    foldlM f acc (x:xs) = do
      acc' <- f acc x
      foldlM f acc' xs

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE PARSING
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse a template for {% include "..." %} directives.
--
-- Returns a list of included template paths.
--
-- @
-- parseTemplateIncludes "{% include \"_shared/context.jinja\" %}"
--   == ["_shared/context.jinja"]
-- @
parseTemplateIncludes :: Text -> [FilePath]
parseTemplateIncludes content =
  case parse includesParser "" content of
    Left _ -> []
    Right paths -> paths

-- | Parse a template for {% extends "..." %} directives.
--
-- Returns a list of extended template paths.
parseTemplateExtends :: Text -> [FilePath]
parseTemplateExtends content =
  case parse extendsParser "" content of
    Left _ -> []
    Right paths -> paths

-- | Parse a template for all dependency directives.
--
-- Returns (includes, extends) tuple.
parseAllDependencies :: Text -> ([FilePath], [FilePath])
parseAllDependencies content =
  (parseTemplateIncludes content, parseTemplateExtends content)

-- ════════════════════════════════════════════════════════════════════════════
-- PARSEC PARSERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parser for all {% include "..." %} directives in a template.
includesParser :: Parser [FilePath]
includesParser = do
  results <- many (try includeDirective <|> skipChar)
  pure [p | Just p <- results]

-- | Parser for all {% extends "..." %} directives in a template.
extendsParser :: Parser [FilePath]
extendsParser = do
  results <- many (try extendsDirective <|> skipChar)
  pure [p | Just p <- results]

-- | Parser for a single {% include "path" %} directive.
includeDirective :: Parser (Maybe FilePath)
includeDirective = do
  _ <- string "{%"
  spaces
  _ <- string "include"
  spaces
  path <- quotedString
  spaces
  _ <- string "%}"
  pure (Just path)

-- | Parser for a single {% extends "path" %} directive.
extendsDirective :: Parser (Maybe FilePath)
extendsDirective = do
  _ <- string "{%"
  spaces
  _ <- string "extends"
  spaces
  path <- quotedString
  spaces
  _ <- string "%}"
  pure (Just path)

-- | Parser for a quoted string (single or double quotes).
quotedString :: Parser FilePath
quotedString = doubleQuoted <|> singleQuoted
  where
    doubleQuoted = do
      _ <- char '"'
      content <- many (noneOf "\"")
      _ <- char '"'
      pure content

    singleQuoted = do
      _ <- char '\''
      content <- many (noneOf "'")
      _ <- char '\''
      pure content

-- | Skip a single character (for scanning through non-directive content).
skipChar :: Parser (Maybe FilePath)
skipChar = do
  _ <- anyChar
  pure Nothing
