{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Graph model for Haiku Explorer.
module ExoMonad.Guest.Graph
  ( -- * Core Types
    Node (..),
    NodeType (..),
    Edge (..),
    Relation (..),
    ExplorationGraph (..),

    -- * Construction
    emptyGraph,
    addNode,
    addEdge,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A node in the exploration graph (symbol, file, concept).
data Node = Node
  { nodeId :: Text,
    nodeType :: NodeType,
    nodeFile :: FilePath,
    nodeLine :: Int,
    nodeSnippet :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Node

instance ToJSON Node

-- | Type of a node.
data NodeType
  = NTFunction
  | NTType
  | NTModule
  | NTFile
  | NTVariable
  | NTConstant
  | NTOther Text
  deriving (Show, Eq, Generic)

instance FromJSON NodeType

instance ToJSON NodeType

-- | An edge between nodes.
data Edge = Edge
  { edgeFrom :: Text,
    edgeTo :: Text,
    edgeRelation :: Relation
  }
  deriving (Show, Eq, Generic)

instance FromJSON Edge

instance ToJSON Edge

-- | Relationship between nodes.
data Relation
  = RCalls
  | RReturns
  | RImports
  | RDefines
  | RUses
  | RInherits
  | RImplement
  | ROther Text
  deriving (Show, Eq, Generic)

instance FromJSON Relation

instance ToJSON Relation

-- | The full exploration graph.
data ExplorationGraph = ExplorationGraph
  { graphNodes :: Map Text Node,
    graphEdges :: [Edge]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExplorationGraph

instance ToJSON ExplorationGraph

-- | Create an empty graph.
emptyGraph :: ExplorationGraph
emptyGraph = ExplorationGraph Map.empty []

-- | Add a node to the graph (idempotent if ID exists).
addNode :: Node -> ExplorationGraph -> ExplorationGraph
addNode node g =
  g {graphNodes = Map.insert (nodeId node) node (graphNodes g)}

-- | Add an edge to the graph.
addEdge :: Edge -> ExplorationGraph -> ExplorationGraph
addEdge edge g =
  g {graphEdges = edge : graphEdges g}
