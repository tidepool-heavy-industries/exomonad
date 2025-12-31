{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Template Haskell for generating typed handler records.
--
-- This module provides TH splices to generate handler record types
-- from Graph definitions. This eliminates boilerplate and ensures
-- type safety between the graph definition and its handlers.
--
-- = Usage
--
-- @
-- type MyGraph = Graph '[
--     Entry :~> InputType
--   , "process" := LLM :@ Needs '[InputType] :@ Schema OutputType
--   , "decide" := Logic :@ Needs '[OutputType] :@ Eff '[Goto Exit Result]
--   , Exit :<~ Result
--   ]
--
-- -- Generate: data Handlers_MyGraph = Handlers_MyGraph { ... }
-- $(deriveHandlers ''MyGraph)
-- @
--
-- = Generated Handler Types
--
-- For LLM nodes:
-- @
-- h_nodeName :: Dep1 -> Dep2 -> ... -> LLM SchemaType
-- @
--
-- For Logic nodes:
-- @
-- h_nodeName :: Dep1 -> Dep2 -> ... -> Eff '[effects...] ()
-- @
--
-- = Limitations
--
-- Currently, this module provides the structure but full implementation
-- requires significant TH machinery to:
--
-- 1. Parse the Graph type structure at compile time
-- 2. Extract node names and annotations
-- 3. Generate the appropriate function types
--
-- For now, users can define handler records manually following the pattern,
-- or use the provided helper functions.
module Tidepool.Graph.TH
  ( -- * Main TH Splice
    deriveHandlers

    -- * Helper Types
  , HandlersFor
  , HandlerType
  ) where

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified as TH
import Data.Kind (Type)
import Effectful qualified

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE FAMILY STUBS
-- ════════════════════════════════════════════════════════════════════════════

-- | Type family to compute the Handlers type for a graph.
--
-- Users can provide instances manually, or TH will generate them.
--
-- @
-- type instance HandlersFor MyGraph = Handlers_MyGraph
-- @
type family HandlersFor (g :: Type) :: Type

-- | Type family to compute a single handler's type.
--
-- @
-- type instance HandlerType MyGraph "process" = InputType -> LLM OutputType
-- @
type family HandlerType (g :: Type) (name :: k) :: Type

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE HASKELL
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a Handlers record for a Graph type.
--
-- This TH splice inspects the Graph type alias and generates:
--
-- 1. A data type @Handlers_GraphName@ with a field per node
-- 2. A @HandlersFor@ type instance
-- 3. Optionally, a @RunnableGraph@ instance
--
-- = Example
--
-- Given:
--
-- @
-- type MyGraph = Graph '[
--     Entry :~> Message
--   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
--   , "route" := Logic :@ Needs '[Message, Intent] :@ Eff '[Goto "a" X, Goto "b" Y]
--   , Exit :<~ Response
--   ]
-- @
--
-- Generates:
--
-- @
-- data Handlers_MyGraph = Handlers_MyGraph
--   { h_classify :: Message -> Eff '[LLM] Intent
--   , h_route :: Message -> Intent -> Eff '[Goto "a" X, Goto "b" Y] ()
--   }
--
-- type instance HandlersFor MyGraph = Handlers_MyGraph
-- @
deriveHandlers :: Name -> Q [Dec]
deriveHandlers graphName = do
  -- Get info about the type
  info <- reify graphName

  case info of
    TyConI (TySynD _ [] graphType) -> do
      -- Parse the Graph structure
      nodes <- parseGraphNodes graphType

      -- Generate the Handlers record
      let handlersName = mkName $ "Handlers_" ++ nameBase graphName
      handlersDec <- generateHandlersRecord handlersName nodes

      -- Generate HandlersFor instance
      let instanceDec = TySynInstD $ TySynEqn Nothing
            (AppT (ConT ''HandlersFor) (ConT graphName))
            (ConT handlersName)

      return $ handlersDec ++ [instanceDec]

    _ -> fail $ "deriveHandlers: " ++ show graphName ++ " is not a type synonym"

-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parsed node information.
data NodeDef = NodeDef
  { ndName :: String          -- ^ Node name
  , ndKind :: NodeKindTH      -- ^ LLM or Logic
  , ndNeeds :: [TH.Type]      -- ^ Types this node needs
  , ndSchema :: Maybe TH.Type -- ^ Schema output (LLM nodes)
  , ndEff :: Maybe TH.Type    -- ^ Effect stack (Logic nodes)
  , ndMemory :: Maybe TH.Type -- ^ Memory type (persistent node state)
  }
  deriving (Show)

data NodeKindTH = LLMTH | LogicTH
  deriving (Show, Eq)

-- | Parse nodes from a Graph type.
parseGraphNodes :: TH.Type -> Q [NodeDef]
parseGraphNodes graphType = do
  -- Extract the node list from Graph '[...]
  case graphType of
    AppT (ConT graphCon) nodeList
      | nameBase graphCon == "Graph" -> parseNodeList nodeList
    -- Handle Graph with annotations: Graph '[...] :& ...
    AppT (AppT (ConT andCon) inner) _
      | nameBase andCon == ":&" -> parseGraphNodes inner
    _ -> fail $ "Unexpected Graph structure: " ++ show graphType

-- | Parse a promoted list of nodes.
parseNodeList :: TH.Type -> Q [NodeDef]
parseNodeList t = case t of
  -- Empty list
  PromotedNilT -> return []

  -- Cons: node ': rest
  AppT (AppT PromotedConsT node) rest -> do
    mDef <- parseNode node
    restDefs <- parseNodeList rest
    return $ maybe restDefs (: restDefs) mDef

  -- Sometimes GHC represents lists differently
  SigT inner _ -> parseNodeList inner

  _ -> fail $ "Unexpected node list structure: " ++ show t

-- | Parse a single node declaration.
-- Returns Nothing for Entry/Exit declarations.
parseNode :: TH.Type -> Q (Maybe NodeDef)
parseNode t = case t of
  -- Entry :~> Type - skip
  AppT (AppT (ConT arrow) (ConT entry)) _
    | nameBase arrow == ":~>" && nameBase entry == "Entry" -> return Nothing

  -- Exit :<~ Type - skip
  AppT (AppT (ConT arrow) (ConT exit)) _
    | nameBase arrow == ":<~" && nameBase exit == "Exit" -> return Nothing

  -- "name" := Kind :@ ...
  _ -> do
    (name, kind, anns) <- parseNodeWithAnnotations t
    return $ Just $ NodeDef
      { ndName = name
      , ndKind = kind
      , ndNeeds = extractNeeds anns
      , ndSchema = extractSchema anns
      , ndEff = extractEff anns
      , ndMemory = extractMemory anns
      }

-- | Parse a node with its annotations.
parseNodeWithAnnotations :: TH.Type -> Q (String, NodeKindTH, [TH.Type])
parseNodeWithAnnotations t = case t of
  -- node :@ annotation
  AppT (AppT (ConT atCon) node) ann
    | nameBase atCon == ":@" -> do
        (name, kind, anns) <- parseNodeWithAnnotations node
        return (name, kind, ann : anns)

  -- "name" := Kind
  AppT (AppT (ConT defCon) (LitT (StrTyLit name))) (PromotedT kind)
    | nameBase defCon == ":=" ->
        return (name, parseKind kind, [])

  AppT (AppT (ConT defCon) (LitT (StrTyLit name))) (ConT kind)
    | nameBase defCon == ":=" ->
        return (name, parseKind kind, [])

  _ -> fail $ "Cannot parse node: " ++ show t

parseKind :: Name -> NodeKindTH
parseKind n = case nameBase n of
  "LLM"   -> LLMTH
  "Logic" -> LogicTH
  _       -> LLMTH  -- Default

-- | Extract Needs types from annotations.
extractNeeds :: [TH.Type] -> [TH.Type]
extractNeeds = concatMap go
  where
    go (AppT (ConT needs) typeList)
      | nameBase needs == "Needs" = extractTypeList typeList
    go _ = []

-- | Extract Schema type from annotations.
extractSchema :: [TH.Type] -> Maybe TH.Type
extractSchema = foldr go Nothing
  where
    go (AppT (ConT schema) t) _
      | nameBase schema == "Schema" = Just t
    go _ acc = acc

-- | Extract UsesEffects stack from annotations.
extractEff :: [TH.Type] -> Maybe TH.Type
extractEff = foldr go Nothing
  where
    go (AppT (ConT eff) t) _
      | nameBase eff == "UsesEffects" = Just t
    go _ acc = acc

-- | Extract Memory type from annotations.
extractMemory :: [TH.Type] -> Maybe TH.Type
extractMemory = foldr go Nothing
  where
    go (AppT (ConT memory) t) _
      | nameBase memory == "Memory" = Just t
    go _ acc = acc

-- | Extract types from a promoted list.
extractTypeList :: TH.Type -> [TH.Type]
extractTypeList t = case t of
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) rest -> x : extractTypeList rest
  SigT inner _ -> extractTypeList inner
  _ -> []

-- | Generate the Handlers record.
generateHandlersRecord :: Name -> [NodeDef] -> Q [Dec]
generateHandlersRecord handlersName nodes = do
  -- Generate record fields
  fields <- mapM generateField nodes

  -- Generate the data declaration
  let dataDec = DataD [] handlersName [] Nothing
        [RecC handlersName fields]
        []  -- No deriving

  return [dataDec]

-- | Generate a single handler field.
generateField :: NodeDef -> Q VarBangType
generateField node = do
  let fieldName = mkName $ "h_" ++ node.ndName

  -- Build the handler type
  handlerType <- buildHandlerType node

  return (fieldName, noBang, handlerType)
  where
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

-- | Build the type for a handler function.
buildHandlerType :: NodeDef -> Q TH.Type
buildHandlerType node = do
  -- Parameters from Needs
  let paramTypes = node.ndNeeds

  -- For effectful, Eff comes from Effectful module
  let effCon = ConT ''Effectful.Eff

  -- Return type depends on node kind
  returnType <- case node.ndKind of
    LLMTH -> case node.ndSchema of
      Just schemaType -> do
        -- LLM nodes return the schema type directly (runner handles LLM effect)
        return schemaType
      Nothing -> [t| () |]
    LogicTH -> case node.ndEff of
      Just effStack -> do
        unitType <- [t| () |]
        return $ AppT (AppT effCon effStack) unitType
      Nothing -> [t| () |]

  -- Build: param1 -> param2 -> ... -> returnType
  return $ foldr (\p acc -> AppT (AppT ArrowT p) acc) returnType paramTypes
