-- | QuickCheck generators for training data.
--
-- Generates synthetic code contexts following patterns from the
-- Tidepool codebase. Uses frequency weighting to produce realistic
-- distributions of location types.
module Tidepool.Training.Arbitrary
  ( -- * Edge generators (for explicit use)
    genEdgeTrainingExample
  , genScoreEdgeInput
  , genScoreEdgeOutput

    -- * Heuristic scoring
  , heuristicScoreEdge
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Tidepool.Training.Types


-- $instances
-- This module provides Arbitrary instances for all training types.
-- Import this module to get generators for 'TrainingExample'.


instance Arbitrary Tag where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink


instance Arbitrary Rubric where
  arbitrary = Rubric
    <$> chooseInt (1, 5)
    <*> chooseInt (1, 5)
    <*> chooseInt (1, 5)
    <*> chooseInt (1, 5)
    <*> (take 4 <$> sublistOf allTags)  -- At most 4 tags

  shrink r = [ r { rTags = ts } | ts <- shrink (rTags r) ]


instance Arbitrary QueryContext where
  arbitrary = QueryContext
    <$> genQuery
    <*> (take 3 <$> sublistOf allTags)  -- 1-3 interest tags

  shrink qc = [ qc { qcTags = ts } | ts <- shrink (qcTags qc) ]


instance Arbitrary NodeContext where
  arbitrary = do
    locType <- frequency
      [ (4, pure PatternMatchLoc)
      , (3, pure TypeFamilyLoc)
      , (3, pure FunctionDefLoc)
      , (2, pure ImportLoc)
      , (2, pure SignatureLoc)
      , (1, pure ConstructorLoc)
      ]
    genNodeContext locType


instance Arbitrary TrainingExample where
  arbitrary = TrainingExample
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary  -- Note: will be replaced by heuristic scorer


-- | Location type for generation.
data LocType
  = PatternMatchLoc
  | TypeFamilyLoc
  | FunctionDefLoc
  | ImportLoc
  | SignatureLoc
  | ConstructorLoc
  deriving (Eq, Show)


-- | Generate a node context for a specific location type.
genNodeContext :: LocType -> Gen NodeContext
genNodeContext locType = do
  moduleName <- elements moduleNames
  lineNum <- chooseInt (10, 500)
  depth <- chooseInt (0, 5)
  breadth <- chooseInt (1, 20)

  (snippet, hover) <- case locType of
    PatternMatchLoc -> genPatternMatch
    TypeFamilyLoc   -> genTypeFamily
    FunctionDefLoc  -> genFunctionDef
    ImportLoc       -> genImport
    SignatureLoc    -> genSignature
    ConstructorLoc  -> genConstructor

  pure NodeContext
    { ncLocation = moduleName <> ".hs:" <> T.pack (show lineNum)
    , ncHover = hover
    , ncCodeSnippet = snippet
    , ncDepth = depth
    , ncBreadth = breadth
    }


-- | Module names from Tidepool codebase.
moduleNames :: [Text]
moduleNames =
  [ "Types", "Edges", "Interpret", "Validate", "Graph"
  , "Reify", "Memory", "Goto", "Execute", "Handler"
  , "Schema", "Template", "Effect", "Tool", "Node"
  ]


-- | Type names used in generated code.
typeNames :: [Text]
typeNames =
  [ "LLMKind", "NodeType", "Effect", "GraphState", "Intent"
  , "GotoTarget", "SchemaType", "ToolDef", "EdgeKind", "Phase"
  ]


-- | Function names used in generated code.
functionNames :: [Text]
functionNames =
  [ "runGraph", "interpret", "dispatch", "validate", "reify"
  , "execute", "handle", "process", "transform", "build"
  ]


-- | Generate realistic query strings.
genQuery :: Gen Text
genQuery = elements
  [ "What breaks if I add a variant to LLMKind?"
  , "Where are exhaustive pattern matches on NodeType?"
  , "What uses the GraphState type?"
  , "Find all type family instances for NodeHandler"
  , "What needs to change if I rename Effect?"
  , "Where is validate called?"
  , "What depends on the Schema module?"
  , "Find recursive functions in the interpreter"
  , "What constructors does Intent have?"
  , "Where is GotoTarget pattern matched?"
  ]


-- | Generate pattern match code snippet and hover.
genPatternMatch :: Gen (Text, Text)
genPatternMatch = do
  typeName <- elements typeNames
  numVariants <- chooseInt (2, 5)
  let variantNames = take numVariants
        ["FirstVariant", "SecondVariant", "ThirdVariant", "FourthVariant", "FifthVariant"]
  let varName = T.toLower (T.take 3 typeName)
  let snippet = "case " <> varName <> " of\n"
             <> T.unlines ["  " <> v <> " -> handleCase" <> v | v <- variantNames]
  let hover = varName <> " :: " <> typeName <> "\nDefined at Types.hs:42"
  pure (snippet, hover)


-- | Generate type family code snippet and hover.
genTypeFamily :: Gen (Text, Text)
genTypeFamily = do
  familyName <- elements
    ["NodeHandler", "GetInput", "HasAnnotation", "DispatchGoto", "ExtractSchema"]
  let snippet = T.unlines
        [ "type family " <> familyName <> " (node :: Type) :: Type where"
        , "  " <> familyName <> " (LLMNode :@ Input i) = i"
        , "  " <> familyName <> " (LogicNode :@ Input i) = i"
        , "  " <> familyName <> " _ = ()"
        ]
  let hover = familyName <> " :: Type -> Type\nType family computing handler input type"
  pure (snippet, hover)


-- | Generate function definition code snippet and hover.
genFunctionDef :: Gen (Text, Text)
genFunctionDef = do
  funcName <- elements functionNames
  typeName <- elements typeNames
  let snippet = T.unlines
        [ funcName <> " :: " <> typeName <> " -> IO Result"
        , funcName <> " input = do"
        , "  result <- processInput input"
        , "  validateResult result"
        , "  pure result"
        ]
  let hover = funcName <> " :: " <> typeName <> " -> IO Result\nProcess and validate input"
  pure (snippet, hover)


-- | Generate import statement code snippet and hover.
genImport :: Gen (Text, Text)
genImport = do
  moduleName <- elements moduleNames
  let snippet = "import Tidepool.Graph." <> moduleName <> " (someExport)"
  let hover = "Module Tidepool.Graph." <> moduleName
  pure (snippet, hover)


-- | Generate type signature code snippet and hover.
genSignature :: Gen (Text, Text)
genSignature = do
  funcName <- elements functionNames
  typeName <- elements typeNames
  let snippet = funcName <> " :: " <> typeName <> " -> Eff effs Result"
  let hover = funcName <> " :: " <> typeName <> " -> Eff effs Result\nNo documentation available"
  pure (snippet, hover)


-- | Generate constructor usage code snippet and hover.
genConstructor :: Gen (Text, Text)
genConstructor = do
  typeName <- elements typeNames
  let conName = typeName <> "Con"
  let snippet = T.unlines
        [ "let value = " <> conName
        , "      { field1 = x"
        , "      , field2 = y"
        , "      }"
        ]
  let hover = conName <> " :: Field1 -> Field2 -> " <> typeName <> "\nData constructor"
  pure (snippet, hover)


-- -----------------------------------------------------------------------------
-- Edge Types (for FunctionGemma 2-turn training)
-- -----------------------------------------------------------------------------

instance Arbitrary EdgeType where
  arbitrary = arbitraryBoundedEnum


instance Arbitrary ScoreEdgeInput where
  arbitrary = genScoreEdgeInput


instance Arbitrary ScoreEdgeOutput where
  arbitrary = genScoreEdgeOutput


instance Arbitrary EdgeTrainingExample where
  arbitrary = genEdgeTrainingExample


-- | Generate a complete edge training example.
--
-- Uses heuristics to score the generated input.
genEdgeTrainingExample :: Gen EdgeTrainingExample
genEdgeTrainingExample = do
  input <- genScoreEdgeInput
  let output = heuristicScoreEdge input
  pure EdgeTrainingExample
    { eteInput = input
    , eteOutput = output
    }


-- | Generate synthetic edge input.
genScoreEdgeInput :: Gen ScoreEdgeInput
genScoreEdgeInput = do
  query <- genQuery
  edgeType <- arbitrary
  (sourceFile, sourceLine, sourceHover) <- genSourceLocation edgeType
  (targetFile, targetLine, targetHover) <- genTargetLocation edgeType
  pure ScoreEdgeInput
    { seiQuery = query
    , seiSourceFile = sourceFile
    , seiSourceLine = sourceLine
    , seiSourceHover = sourceHover
    , seiTargetFile = targetFile
    , seiTargetLine = targetLine
    , seiTargetHover = targetHover
    , seiEdgeType = edgeType
    }


-- | Generate heuristic-scored output for an edge.
--
-- This is the ground truth we're training the model to produce.
genScoreEdgeOutput :: Gen ScoreEdgeOutput
genScoreEdgeOutput = do
  input <- genScoreEdgeInput
  pure $ heuristicScoreEdge input


-- | Heuristic scorer for edges (to be replaced by trained model).
heuristicScoreEdge :: ScoreEdgeInput -> ScoreEdgeOutput
heuristicScoreEdge input = ScoreEdgeOutput
  { seoRelevance = computeEdgeRelevance input
  , seoRisk = computeEdgeRisk input
  , seoReasoning = generateReasoning input
  , seoIsExhaustive = detectExhaustive input
  , seoIsTypeFamily = detectTypeFamily input
  , seoIsExported = detectExported input
  }


-- | Compute relevance based on query and edge type.
computeEdgeRelevance :: ScoreEdgeInput -> Int
computeEdgeRelevance input
  -- Definition edges for "where is X defined" queries
  | "defined" `T.isInfixOf` queryLower && input.seiEdgeType == Definition = 5
  | "where is" `T.isInfixOf` queryLower && input.seiEdgeType == Definition = 5
  -- Reference edges for "what uses X" queries
  | "uses" `T.isInfixOf` queryLower && input.seiEdgeType == Reference = 5
  | "depends" `T.isInfixOf` queryLower && input.seiEdgeType == Reference = 5
  -- Breaking change queries
  | "breaks" `T.isInfixOf` queryLower && hasPatternMatch = 5
  | "add" `T.isInfixOf` queryLower && "variant" `T.isInfixOf` queryLower && hasPatternMatch = 5
  -- Type family queries
  | "type family" `T.isInfixOf` queryLower && hasTypeFamily = 5
  -- Default based on edge type
  | input.seiEdgeType == Definition = 4
  | input.seiEdgeType == Reference = 3
  | input.seiEdgeType == Instance = 4
  | otherwise = 3
  where
    queryLower = T.toLower input.seiQuery
    hoverLower = T.toLower input.seiTargetHover
    hasPatternMatch = "case " `T.isInfixOf` hoverLower || "pattern" `T.isInfixOf` hoverLower
    hasTypeFamily = "type family" `T.isInfixOf` hoverLower


-- | Compute risk based on target hover info.
computeEdgeRisk :: ScoreEdgeInput -> Int
computeEdgeRisk input
  -- High risk: exhaustive pattern matches
  | "case " `T.isInfixOf` hoverLower && T.count "->" hoverLower >= 2 = 5
  -- High risk: type families
  | "type family" `T.isInfixOf` hoverLower = 5
  -- Medium-high risk: exported symbols
  | "exported" `T.isInfixOf` hoverLower || isLikelyExported = 4
  -- Medium risk: regular pattern match
  | "case " `T.isInfixOf` hoverLower = 4
  -- Medium risk: implementation
  | "=" `T.isInfixOf` hoverLower && "::" `T.isInfixOf` hoverLower = 3
  -- Low risk: imports
  | "import " `T.isInfixOf` hoverLower = 1
  | otherwise = 3
  where
    hoverLower = T.toLower input.seiTargetHover
    -- Heuristic: functions in "public" modules are likely exported
    isLikelyExported = any (`T.isInfixOf` input.seiTargetFile)
      ["Types.hs", "API.hs", "Public.hs", "Export.hs"]


-- | Generate reasoning text based on detected patterns.
generateReasoning :: ScoreEdgeInput -> Text
generateReasoning input
  | detectExhaustive input =
      "Pattern match on " <> extractTypeName input.seiTargetHover <>
      " - must add case for new variant"
  | detectTypeFamily input =
      "Type family instance - type-level computation may need update"
  | input.seiEdgeType == Definition =
      "Definition site for queried symbol"
  | input.seiEdgeType == Reference =
      "Reference to queried symbol"
  | input.seiEdgeType == Instance =
      "Implementation of interface"
  | otherwise =
      "Related code location"


-- | Detect exhaustive pattern match.
detectExhaustive :: ScoreEdgeInput -> Bool
detectExhaustive input =
  let hover = T.toLower input.seiTargetHover
  in "case " `T.isInfixOf` hover && T.count "->" hover >= 3


-- | Detect type family.
detectTypeFamily :: ScoreEdgeInput -> Bool
detectTypeFamily input =
  "type family" `T.isInfixOf` T.toLower input.seiTargetHover


-- | Detect likely exported symbol.
detectExported :: ScoreEdgeInput -> Bool
detectExported input =
  let file = T.toLower input.seiTargetFile
      hover = T.toLower input.seiTargetHover
  in "exported" `T.isInfixOf` hover
     || any (`T.isInfixOf` file) ["types.hs", "api.hs", "public.hs"]
     || not ("_" `T.isPrefixOf` extractFuncName hover)


-- | Extract type name from hover info.
extractTypeName :: Text -> Text
extractTypeName hover =
  case T.words hover of
    ("data":name:_) -> name
    ("type":name:_) -> name
    _ -> "symbol"


-- | Extract function name from hover info.
extractFuncName :: Text -> Text
extractFuncName hover =
  case T.words hover of
    (name:_) -> name
    _ -> "func"


-- | Generate source location based on edge type.
genSourceLocation :: EdgeType -> Gen (Text, Int, Text)
genSourceLocation edgeType = do
  moduleName <- elements moduleNames
  lineNum <- chooseInt (10, 500)
  typeName <- elements typeNames
  let file = moduleName <> ".hs"
  let hover = case edgeType of
        Definition -> typeName <> " :: SomeType\nDefined here"
        Reference -> "Referencing " <> typeName
        Usage -> "Calling function using " <> typeName
        Instance -> "instance SomeClass " <> typeName
        TypeConstraint -> "constraint: " <> typeName <> " a"
  pure (file, lineNum, hover)


-- | Generate target location based on edge type.
genTargetLocation :: EdgeType -> Gen (Text, Int, Text)
genTargetLocation edgeType = do
  moduleName <- elements moduleNames
  lineNum <- chooseInt (10, 500)
  typeName <- elements typeNames

  let file = moduleName <> ".hs"
  hover <- case edgeType of
    Definition -> do
      -- Generate data type or function definition
      frequency
        [ (3, pure $ "data " <> typeName <> " = Variant1 | Variant2 | Variant3")
        , (2, pure $ typeName <> " :: SomeInput -> IO Result\nProcess input")
        , (1, pure $ "type family " <> typeName <> " a :: Type")
        ]
    Reference -> do
      -- Generate pattern match or usage site
      frequency
        [ (4, pure $ "case x of\n  Variant1 -> handle1\n  Variant2 -> handle2\n  Variant3 -> handle3")
        , (2, pure $ "process :: " <> typeName <> " -> Result\nExported function")
        ]
    Usage ->
      pure $ "callSite = someFunc " <> T.toLower typeName <> "Value"
    Instance ->
      pure $ "instance Handler " <> typeName <> " where\n  handle = ..."
    TypeConstraint ->
      pure $ "constrained :: " <> typeName <> " a => a -> Result"

  pure (file, lineNum, hover)
