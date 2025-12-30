{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid diagram generation for Graph definitions.
--
-- Generates Mermaid flowchart syntax from reified graph information.
-- The output can be rendered by any Mermaid-compatible tool.
--
-- = Node Shapes
--
-- * Entry/Exit: @((circle))@
-- * LLM nodes: @[[double brackets]]@
-- * Logic nodes: @{{hexagon}}@
--
-- = Edge Styles
--
-- * Implicit (Schema → Needs): @-->@ solid arrow
-- * Explicit (Goto): @-->@ solid arrow
-- * Conditional (from When): @-.->@ dashed arrow
--
-- = Example Output
--
-- @
-- flowchart TD
--     entry((start)) -->|Document| classify
--     classify[[\"classify<br/>LLM\"]] -->|Intent| route
--     route{{\"route<br/>Logic\"}} -->|Message| handler
--     handler[[\"handler<br/>LLM\"]] -->|Response| exit((end))
-- @
module Tidepool.Graph.Mermaid
  ( -- * Diagram Generation
    toMermaid
  , toMermaidWithConfig

    -- * Configuration
  , MermaidConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep)

import Tidepool.Graph.Reify

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Mermaid output.
data MermaidConfig = MermaidConfig
  { mcDirection :: Text          -- ^ Flow direction: TD (top-down), LR (left-right)
  , mcShowTypes :: Bool          -- ^ Show type names on edges
  , mcShowNodeKind :: Bool       -- ^ Show LLM/Logic labels in nodes
  , mcEntryLabel :: Text         -- ^ Label for entry node
  , mcExitLabel :: Text          -- ^ Label for exit node
  }
  deriving (Show, Eq)

-- | Default configuration.
defaultConfig :: MermaidConfig
defaultConfig = MermaidConfig
  { mcDirection = "TD"
  , mcShowTypes = True
  , mcShowNodeKind = True
  , mcEntryLabel = "start"
  , mcExitLabel = "end"
  }

-- ════════════════════════════════════════════════════════════════════════════
-- DIAGRAM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate Mermaid diagram with default configuration.
toMermaid :: GraphInfo -> Text
toMermaid = toMermaidWithConfig defaultConfig

-- | Generate Mermaid diagram with custom configuration.
toMermaidWithConfig :: MermaidConfig -> GraphInfo -> Text
toMermaidWithConfig config info = T.unlines $
  [ "flowchart " <> config.mcDirection
  , ""
  , "    %% Entry and Exit"
  , "    entry((" <> config.mcEntryLabel <> "))"
  , "    exit__((" <> config.mcExitLabel <> "))"
  , ""
  ]
  ++ nodeDeclarations config info
  ++ [""]
  ++ groupDeclarations info
  ++ [""]
  ++ edgeDeclarations config info

-- | Generate node declarations.
nodeDeclarations :: MermaidConfig -> GraphInfo -> [Text]
nodeDeclarations config info =
  ["    %% Nodes"] ++ map (renderNode config) info.giNodes

-- | Render a single node declaration.
renderNode :: MermaidConfig -> NodeInfo -> Text
renderNode config node =
  "    " <> escapeName node.niName <> shape
  where
    label = if config.mcShowNodeKind
            then node.niName <> "<br/>" <> kindLabel node.niKind
            else node.niName
    shape = case node.niKind of
      RuntimeLLM   -> "[[\"" <> label <> "\"]]"
      RuntimeLogic -> "{{\"" <> label <> "\"}}"

    kindLabel RuntimeLLM   = "LLM"
    kindLabel RuntimeLogic = "Logic"

-- | Generate group (subgraph) declarations.
groupDeclarations :: GraphInfo -> [Text]
groupDeclarations info
  | null info.giGroups = []
  | otherwise =
      ["    %% Groups"] ++ concatMap renderGroup info.giGroups

-- | Render a subgraph.
renderGroup :: (Text, [Text]) -> [Text]
renderGroup (name, members) =
  [ "    subgraph " <> escapeName name
  ] ++ map (\m -> "        " <> escapeName m) members ++
  [ "    end"
  ]

-- | Generate edge declarations.
edgeDeclarations :: MermaidConfig -> GraphInfo -> [Text]
edgeDeclarations config info =
  ["    %% Edges"]
  ++ entryEdges config info
  ++ nodeEdges config info
  ++ exitEdges config info

-- | Generate edges from Entry to nodes that need the entry type.
entryEdges :: MermaidConfig -> GraphInfo -> [Text]
entryEdges config info = case info.giEntryType of
  Nothing -> []
  Just entryType ->
    [ "    entry" <> arrow <> "|" <> typeLabel config entryType <> "| " <> escapeName node.niName
    | node <- info.giNodes
    , entryType `elem` node.niNeeds
    ]
  where
    arrow = " --> "

-- | Generate edges between nodes.
nodeEdges :: MermaidConfig -> GraphInfo -> [Text]
nodeEdges config info =
  concatMap (nodeToNodeEdges config info) info.giNodes

-- | Generate edges from a single node.
nodeToNodeEdges :: MermaidConfig -> GraphInfo -> NodeInfo -> [Text]
nodeToNodeEdges config info node =
  schemaEdges ++ gotoEdges
  where
    -- Schema → Needs edges (implicit)
    schemaEdges = case node.niSchema of
      Nothing -> []
      Just schemaType ->
        [ "    " <> escapeName node.niName <> arrow <> "|" <> typeLabel config schemaType <> "| " <> escapeName target.niName
        | target <- info.giNodes
        , schemaType `elem` target.niNeeds
        , target.niName /= node.niName  -- No self-loops from schema
        ]
        where
          arrow = if node.niIsConditional then " -.-> " else " --> "

    -- Goto edges (explicit)
    gotoEdges =
      [ "    " <> escapeName node.niName <> " --> |" <> typeLabel config payload <> "| " <> escapeName targetName
      | (targetName, payload) <- node.niGotoTargets
      ]

-- | Generate edges to Exit.
exitEdges :: MermaidConfig -> GraphInfo -> [Text]
exitEdges config info = case info.giExitType of
  Nothing ->
    [ "    " <> escapeName node.niName <> " --> exit__"
    | node <- info.giNodes
    , node.niHasGotoExit
    ]
  Just exitType ->
    [ "    " <> escapeName node.niName <> " --> |" <> typeLabel config exitType <> "| exit__"
    | node <- info.giNodes
    , node.niHasGotoExit
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Escape a node name for Mermaid (handle special characters).
escapeName :: Text -> Text
escapeName name
  | T.any needsEscape name = "\"" <> T.replace "\"" "\\\"" name <> "\""
  | otherwise = name
  where
    needsEscape c = c `elem` (" -()[]{}<>\"'" :: String)

-- | Generate type label for an edge.
typeLabel :: MermaidConfig -> TypeRep -> Text
typeLabel config tr
  | config.mcShowTypes = simplifyType $ T.pack $ show tr
  | otherwise = ""

-- | Simplify a type representation for display.
--
-- Removes module prefixes and simplifies common patterns.
simplifyType :: Text -> Text
simplifyType t = T.pack $ go $ T.unpack t
  where
    go s = case break (== '.') s of
      (_, []) -> s
      (_, '.':rest) ->
        -- Check if next segment is uppercase (module) or lowercase (end)
        case rest of
          (c:_) | c >= 'A' && c <= 'Z' -> go rest
          _ -> s
      _ -> s
