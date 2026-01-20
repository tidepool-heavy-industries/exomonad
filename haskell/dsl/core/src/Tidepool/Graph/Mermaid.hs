
-- | Mermaid diagram generation for Graph definitions.
--
-- Generates Mermaid flowchart syntax from reified graph information.
-- The output can be rendered by any Mermaid-compatible tool.
--
-- = Node Shapes
--
-- * EntryNode/Exit: @((circle))@
-- * LLM nodes: @[[double brackets]]@
-- * Logic nodes: @{{hexagon}}@
--
-- = Edge Styles
--
-- * Implicit (Schema → Input): @-->@ solid arrow
-- * Explicit (Goto): @-->@ solid arrow
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
  ( -- * Flowchart Generation
    toMermaid
  , toMermaidWithConfig
  , graphToMermaid
  , graphToMermaidWithConfig

    -- * State Diagram Generation
  , toStateDiagram
  , toStateDiagramWithConfig

    -- * Sequence Diagram Generation
  , toSequenceDiagram
  , ExecutionPath(..)

    -- * Configuration
  , MermaidConfig(..)
  , defaultConfig
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Char (isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep)
import GHC.Generics (Generic(..))

import Tidepool.Graph.Generic.Core (AsGraph)
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
  , mcIncludeComments :: Bool    -- ^ Include structured comments for LLM context
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
  , mcIncludeComments = True
  }

-- ════════════════════════════════════════════════════════════════════════════
-- DIAGRAM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate Mermaid diagram with default configuration.
toMermaid :: GraphInfo -> Text
toMermaid = toMermaidWithConfig defaultConfig

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-TO-MERMAID (TYPE-DRIVEN)
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate Mermaid diagram directly from a graph type.
--
-- This is a convenience function that combines 'makeGraphInfo' with 'toMermaid'.
-- It extracts graph structure at compile time and generates the diagram.
--
-- @
-- data SupportGraph mode = SupportGraph
--   { sgEntry    :: mode :- EntryNode Message
--   , sgClassify :: mode :- LLMNode :@ Input Message :@ Schema Intent
--   , sgRoute    :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "sgRefund" Message, Goto "sgFaq" Message]
--   , sgRefund   :: mode :- LLMNode :@ Input Message :@ Schema Response
--   , sgFaq      :: mode :- LLMNode :@ Input Message :@ Schema Response
--   , sgExit     :: mode :- ExitNode Response
--   }
--   deriving Generic
--
-- -- Generate Mermaid diagram:
-- mermaidOutput :: Text
-- mermaidOutput = graphToMermaid (Proxy @SupportGraph)
-- @
graphToMermaid
  :: forall (graph :: Type -> Type).
     ( GReifyFields (Rep (graph AsGraph))
     , ReifyMaybeType (GetEntryTypeFromGraph graph)
     , ReifyMaybeType (GetExitTypeFromGraph graph)
     )
  => Proxy graph
  -> Text
graphToMermaid p = toMermaid (makeGraphInfo p)

-- | Generate Mermaid diagram with custom configuration from a graph type.
graphToMermaidWithConfig
  :: forall (graph :: Type -> Type).
     ( GReifyFields (Rep (graph AsGraph))
     , ReifyMaybeType (GetEntryTypeFromGraph graph)
     , ReifyMaybeType (GetExitTypeFromGraph graph)
     )
  => MermaidConfig
  -> Proxy graph
  -> Text
graphToMermaidWithConfig config p = toMermaidWithConfig config (makeGraphInfo p)

-- ════════════════════════════════════════════════════════════════════════════
-- FLOWCHART GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate Mermaid diagram with custom configuration.
toMermaidWithConfig :: MermaidConfig -> GraphInfo -> Text
toMermaidWithConfig config info = T.unlines $
  [ "flowchart " <> config.mcDirection
  , ""
  , "    %% EntryNode and Exit"
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
  "    %% Nodes" : concatMap (renderNodeWithComments config) info.giNodes

-- | Render a node with optional structured comments.
renderNodeWithComments :: MermaidConfig -> NodeInfo -> [Text]
renderNodeWithComments config node =
  comments ++ [declaration]
  where
    comments = if config.mcIncludeComments
               then renderNodeComments node
               else []
    declaration = renderNode config node

-- | Render structured comments for a node (for LLM context).
renderNodeComments :: NodeInfo -> [Text]
renderNodeComments node = filter (not . T.null)
  [ "    %% NODE: " <> node.niName
  , "    %% kind: " <> kindText node.niKind
  , renderClaudeCodeComment node.niClaudeCode
  , renderGeminiComment node.niGemini
  , renderTemplateComment node.niTemplate
  , renderSchemaComment node.niSchema
  , renderToolsComment node.niToolInfos
  , renderMemoryComment node.niMemory
  , renderTransitionsComment node
  ]
  where
    kindText RuntimeLLM = "LLM"
    kindText RuntimeClaudeCode = "ClaudeCode"
    kindText RuntimeGemini = "Gemini"
    kindText RuntimeLogic = "Logic"
    kindText RuntimeFork = "Fork"
    kindText RuntimeBarrier = "Barrier"

-- | Render template comment.
renderTemplateComment :: Maybe TemplateInfo -> Text
renderTemplateComment Nothing = ""
renderTemplateComment (Just ti) = T.intercalate "\n"
  [ "    %% template: " <> T.pack ti.tiPath
  , "    %%   deps: " <> T.pack (show ti.tiDeps)
  , "    %%   fields: " <> T.pack (show ti.tiAccessedFields)
  , "    %%   context: " <> ti.tiContextType
  ]

-- | Render schema comment.
renderSchemaComment :: Maybe SchemaInfo -> Text
renderSchemaComment Nothing = ""
renderSchemaComment (Just si) = T.intercalate "\n"
  [ "    %% schema: " <> si.siTypeName
  , "    %%   fields: " <> renderFields si.siFields
  ]
  where
    renderFields = T.intercalate ", " . map (\(n,t,_) -> n <> ": " <> t)

-- | Render tools comment.
renderToolsComment :: [ToolInfo] -> Text
renderToolsComment [] = "    %% tools: none"
renderToolsComment tools = "    %% tools: " <> T.intercalate ", " (map (.tiName) tools)

-- | Render memory comment.
renderMemoryComment :: Maybe MemoryInfo -> Text
renderMemoryComment Nothing = ""
renderMemoryComment (Just mi) = "    %% memory: " <> mi.miTypeName

-- | Render ClaudeCode comment.
renderClaudeCodeComment :: Maybe ClaudeCodeInfo -> Text
renderClaudeCodeComment Nothing = ""
renderClaudeCodeComment (Just cci) = "    %% claudeCode: " <> cci.cciModel

-- | Render Gemini comment.
renderGeminiComment :: Maybe GeminiInfo -> Text
renderGeminiComment Nothing = ""
renderGeminiComment (Just gi) = "    %% gemini: " <> gi.giModel

-- | Render transitions comment.
renderTransitionsComment :: NodeInfo -> Text
renderTransitionsComment node
  | null node.niGotoTargets && not node.niHasGotoExit = ""
  | otherwise = "    %% transitions: " <> T.intercalate ", " targets
  where
    targets = map fst node.niGotoTargets
           ++ ["Exit" | node.niHasGotoExit]

-- | Render a single node declaration.
renderNode :: MermaidConfig -> NodeInfo -> Text
renderNode config node =
  "    " <> escapeName node.niName <> shape
  where
    label = if config.mcShowNodeKind
            then node.niName <> "<br/>" <> kindLabel node
            else node.niName
    shape = case node.niKind of
      RuntimeLLM        -> "[[\"" <> label <> "\"]]"
      RuntimeClaudeCode -> "[[\"" <> label <> "\"]]"  -- Same shape as LLM
      RuntimeGemini     -> "[[\"" <> label <> "\"]]"  -- Same shape as LLM
      RuntimeLogic      -> "{{\"" <> label <> "\"}}"
      RuntimeFork       -> "{\"" <> label <> "\"}"     -- Diamond for fork
      RuntimeBarrier    -> "[/\"" <> label <> "\"/]"   -- Trapezoid for barrier

    -- For ClaudeCode nodes, include the model name in the label
    kindLabel n = case n.niKind of
      RuntimeLLM -> "LLM"
      RuntimeClaudeCode -> case n.niClaudeCode of
        Just cci -> "CC " <> cci.cciModel  -- e.g., "CC Sonnet"
        Nothing  -> "ClaudeCode"
      RuntimeGemini -> case n.niGemini of
        Just gi -> "Gemini " <> gi.giModel  -- e.g., "Gemini Flash"
        Nothing -> "Gemini"
      RuntimeLogic -> "Logic"
      RuntimeFork -> "Fork"
      RuntimeBarrier -> "Barrier"

-- | Generate group (subgraph) declarations.
groupDeclarations :: GraphInfo -> [Text]
groupDeclarations info
  | null info.giGroups = []
  | otherwise =
      "    %% Groups" : concatMap renderGroup info.giGroups

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

-- | Generate edges from EntryNode to root nodes.
--
-- Root nodes are nodes with no incoming edges (Schema→Needs or Goto)
-- from other nodes. These are the natural entry points of the graph.
entryEdges :: MermaidConfig -> GraphInfo -> [Text]
entryEdges config info = case info.giEntryType of
  Nothing -> []
  Just entryType ->
    [ "    entry --> |" <> typeLabel config entryType <> "| " <> escapeName name
    | name <- findRootNodes info
    ]

-- | Generate edges between nodes.
nodeEdges :: MermaidConfig -> GraphInfo -> [Text]
nodeEdges config info =
  concatMap (nodeToNodeEdges config info) info.giNodes

-- | Generate edges from a single node.
nodeToNodeEdges :: MermaidConfig -> GraphInfo -> NodeInfo -> [Text]
nodeToNodeEdges config info node =
  schemaEdges ++ gotoEdges
  where
    -- Schema → Input edges (implicit)
    schemaEdges = case node.niSchema of
      Nothing -> []
      Just schemaInfo ->
        [ "    " <> escapeName node.niName <> " --> |" <> typeLabel config schemaInfo.siType <> "| " <> escapeName target.niName
        | target <- info.giNodes
        , target.niInput == Just schemaInfo.siType
        , target.niName /= node.niName  -- No self-loops from schema
        ]

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
          (c:_) | isAsciiUpper c -> go rest
          _ -> s
      _ -> s

-- ════════════════════════════════════════════════════════════════════════════
-- STATE DIAGRAM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate Mermaid state diagram with default configuration.
--
-- State diagrams are semantically accurate for our Graph DSL since
-- nodes ARE states and Goto transitions ARE state transitions.
--
-- @
-- stateDiagram-v2
--     [*] --> classify: Message
--     classify --> route: Intent
--     route --> refund: Message
--     route --> answer: Message
--     refund --> [*]: Response
--     answer --> [*]: Response
-- @
toStateDiagram :: GraphInfo -> Text
toStateDiagram = toStateDiagramWithConfig defaultConfig

-- | Generate Mermaid state diagram with custom configuration.
toStateDiagramWithConfig :: MermaidConfig -> GraphInfo -> Text
toStateDiagramWithConfig config info = T.unlines $
  [ "stateDiagram-v2"
  , ""
  ]
  ++ stateDefinitions config info
  ++ [""]
  ++ stateTransitions config info

-- | Generate state definitions with annotations.
stateDefinitions :: MermaidConfig -> GraphInfo -> [Text]
stateDefinitions config info =
  "    %% State definitions"
  : map (renderState config) info.giNodes

-- | Render a state definition.
renderState :: MermaidConfig -> NodeInfo -> Text
renderState config node =
  if config.mcShowNodeKind
  then "    " <> escapeName node.niName <> " : " <> kindAnnotation node
  else ""
  where
    kindAnnotation n = case n.niKind of
      RuntimeLLM -> "LLM"
      RuntimeClaudeCode -> case n.niClaudeCode of
        Just cci -> "CC " <> cci.cciModel
        Nothing  -> "ClaudeCode"
      RuntimeGemini -> case n.niGemini of
        Just gi -> "Gemini " <> gi.giModel
        Nothing -> "Gemini"
      RuntimeLogic -> "Logic"
      RuntimeFork -> "Fork"
      RuntimeBarrier -> "Barrier"

-- | Generate state transitions.
stateTransitions :: MermaidConfig -> GraphInfo -> [Text]
stateTransitions config info =
  ["    %% Transitions"]
  ++ entryStateTransitions config info
  ++ nodeStateTransitions config info
  ++ exitStateTransitions config info

-- | EntryNode transitions.
--
-- Connects to root nodes (nodes with no incoming edges from other nodes).
entryStateTransitions :: MermaidConfig -> GraphInfo -> [Text]
entryStateTransitions config info = case info.giEntryType of
  Nothing -> []
  Just entryType ->
    [ "    [*] --> " <> escapeName name <> ": " <> typeLabel config entryType
    | name <- findRootNodes info
    ]

-- | Node-to-node transitions.
nodeStateTransitions :: MermaidConfig -> GraphInfo -> [Text]
nodeStateTransitions config info =
  concatMap (nodeStateEdges config info) info.giNodes

-- | Transitions from a single node.
nodeStateEdges :: MermaidConfig -> GraphInfo -> NodeInfo -> [Text]
nodeStateEdges config info node =
  schemaTransitions ++ gotoTransitions
  where
    -- Schema → Input transitions (implicit data flow)
    schemaTransitions = case node.niSchema of
      Nothing -> []
      Just schemaInfo ->
        [ "    " <> escapeName node.niName <> " --> " <> escapeName target.niName <> ": " <> typeLabel config schemaInfo.siType
        | target <- info.giNodes
        , target.niInput == Just schemaInfo.siType
        , target.niName /= node.niName
        ]

    -- Goto transitions (explicit control flow)
    gotoTransitions =
      [ "    " <> escapeName node.niName <> " --> " <> escapeName targetName <> ": " <> typeLabel config payload
      | (targetName, payload) <- node.niGotoTargets
      ]

-- | Exit transitions.
exitStateTransitions :: MermaidConfig -> GraphInfo -> [Text]
exitStateTransitions config info = case info.giExitType of
  Nothing ->
    [ "    " <> escapeName node.niName <> " --> [*]"
    | node <- info.giNodes
    , node.niHasGotoExit
    ]
  Just exitType ->
    [ "    " <> escapeName node.niName <> " --> [*]: " <> typeLabel config exitType
    | node <- info.giNodes
    , node.niHasGotoExit
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- SEQUENCE DIAGRAM GENERATION
-- ════════════════════════════════════════════════════════════════════════════

-- | An execution path through the graph.
--
-- Each step is a node name. The path starts implicitly from EntryNode
-- and ends at Exit.
newtype ExecutionPath = ExecutionPath { pathSteps :: [Text] }
  deriving (Show, Eq)

-- | Generate Mermaid sequence diagram for a specific execution path.
--
-- Since graphs can have branches, you must specify which path to visualize.
--
-- @
-- sequenceDiagram
--     participant EntryNode
--     participant classify
--     participant route
--     participant refund
--     participant Exit
--
--     EntryNode->>classify: Message
--     classify->>route: Intent
--     route->>refund: Message
--     refund->>Exit: Response
-- @
toSequenceDiagram :: MermaidConfig -> GraphInfo -> ExecutionPath -> Text
toSequenceDiagram config info path = T.unlines $
  [ "sequenceDiagram"
  , ""
  , "    %% Participants"
  , "    participant EntryNode"
  ]
  ++ map renderParticipant path.pathSteps
  ++ [ "    participant Exit"
     , ""
     , "    %% Message flow"
     ]
  ++ sequenceMessages config info path

-- | Render a participant declaration.
renderParticipant :: Text -> Text
renderParticipant name = "    participant " <> escapeName name

-- | Generate sequence messages for the path.
sequenceMessages :: MermaidConfig -> GraphInfo -> ExecutionPath -> [Text]
sequenceMessages config info (ExecutionPath steps) =
  entryMessage ++ stepMessages ++ exitMessage
  where
    -- EntryNode to first node
    entryMessage = case (steps, info.giEntryType) of
      (first:_, Just entryType) ->
        ["    EntryNode->>" <> escapeName first <> ": " <> typeLabel config entryType]
      _ -> []

    -- Messages between consecutive nodes
    stepMessages = zipWith (makeMessage config info) steps (drop 1 steps)

    -- Last node to Exit
    exitMessage = case (reverse steps, info.giExitType) of
      (lastNode:_, Just exitType) ->
        ["    " <> escapeName lastNode <> "->>Exit: " <> typeLabel config exitType]
      (lastNode:_, Nothing) ->
        ["    " <> escapeName lastNode <> "->>Exit: "]
      _ -> []

-- | Make a message between two nodes.
makeMessage :: MermaidConfig -> GraphInfo -> Text -> Text -> Text
makeMessage config info fromNode toNode =
  "    " <> escapeName fromNode <> "->>" <> escapeName toNode <> ": " <> msgType
  where
    -- Find the type being passed (from Schema or Goto)
    msgType = case findNodeByName info fromNode of
      Nothing -> ""
      Just node ->
        -- Check if it's a Goto transition
        case lookup toNode node.niGotoTargets of
          Just payload -> typeLabel config payload
          Nothing ->
            -- Must be Schema → Needs
            case node.niSchema of
              Just schemaInfo -> typeLabel config schemaInfo.siType
              Nothing -> ""

-- | Find a node by name.
findNodeByName :: GraphInfo -> Text -> Maybe NodeInfo
findNodeByName info name =
  case filter (\n -> n.niName == name) info.giNodes of
    [n] -> Just n
    _ -> Nothing

-- | Find root nodes (nodes with no incoming edges from other nodes).
--
-- These are the natural entry points of the graph.
findRootNodes :: GraphInfo -> [Text]
findRootNodes info =
  [ node.niName
  | node <- info.giNodes
  , not (hasIncomingEdge info node.niName)
  ]

-- | Check if a node has any incoming edge (Schema→Needs or Goto).
hasIncomingEdge :: GraphInfo -> Text -> Bool
hasIncomingEdge info targetName = any sendsTo info.giNodes
  where
    sendsTo node =
      -- Goto edge to this node?
      targetName `elem` map fst node.niGotoTargets ||
      -- Schema→Input edge to this node?
      case node.niSchema of
        Nothing -> False
        Just schemaInfo ->
          case findNodeByName info targetName of
            Nothing -> False
            Just target -> target.niInput == Just schemaInfo.siType
