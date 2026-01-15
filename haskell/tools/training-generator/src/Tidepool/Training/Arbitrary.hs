-- | QuickCheck generators for training data.
--
-- Generates synthetic code contexts following patterns from the
-- Tidepool codebase. Uses frequency weighting to produce realistic
-- distributions of location types.
module Tidepool.Training.Arbitrary
  ( -- * Re-export Arbitrary instances
    -- $instances
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
