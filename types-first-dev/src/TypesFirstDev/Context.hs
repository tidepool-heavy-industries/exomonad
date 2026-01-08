{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template context types for the types-first workflow.
--
-- This module is separate from Templates.hs so that TH splices can reference
-- these types (TH staging requires types to be in previously compiled modules).
--
-- Context types map to Jinja template variables.
module TypesFirstDev.Context
  ( TypesContext(..)
  , TestsContext(..)
  , ImplContext(..)
  , SkeletonContext(..)
    -- * v3 Stubs Workflow
  , StubsContext(..)
  , TestsContextV3(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import TypesFirstDev.Types (FunctionSig(..), TestPriority(..), FunctionSemantics(..), FunctionExample(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the types template.
--
-- Template variables:
--   {{ moduleName }} - e.g., "Data.Stack"
--   {{ description }} - natural language description
--   {{ acceptanceCriteria }} - list of acceptance criteria strings
data TypesContext = TypesContext
  { moduleName :: Text
    -- ^ Module name for the data structure.
  , description :: Text
    -- ^ Natural language description.
  , acceptanceCriteria :: [Text]
    -- ^ High-level acceptance criteria to inform test priorities.
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) TypesContext where
  toGVal ctx = dict
    [ ("moduleName", toGVal ctx.moduleName)
    , ("description", toGVal ctx.description)
    , ("acceptanceCriteria", list $ map toGVal ctx.acceptanceCriteria)
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the tests template.
--
-- Template variables:
--   {{ moduleName }} - module name
--   {{ dataType }} - data type definition
--   {{ signatures }} - list of FunctionSig objects with .name, .signature, .description
--   {{ testPriorities }} - list of TestPriority objects with .name, .description
data TestsContext = TestsContext
  { moduleName :: Text
  , dataType :: Text
  , signatures :: [FunctionSig]
  , testPriorities :: [TestPriority]
  }
  deriving (Show, Eq, Generic)

-- | ToGVal for FunctionSig - exposes name, signature, description to templates
instance ToGVal (Run SourcePos (Writer Text) Text) FunctionSig where
  toGVal sig = dict
    [ ("name", toGVal sig.fsName)
    , ("signature", toGVal sig.fsSignature)
    , ("description", toGVal sig.fsDescription)
    ]

instance ToGVal (Run SourcePos (Writer Text) Text) TestsContext where
  toGVal TestsContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    , ("testPriorities", list $ map toGVal testPriorities)
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the impl template.
--
-- Template variables:
--   {{ moduleName }} - module name
--   {{ dataType }} - data type definition
--   {{ signatures }} - list of FunctionSig objects with .name, .signature, .description
data ImplContext = ImplContext
  { moduleName :: Text
  , dataType :: Text
  , signatures :: [FunctionSig]
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) ImplContext where
  toGVal ImplContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- SKELETON TEMPLATE CONTEXT (v2)
-- ════════════════════════════════════════════════════════════════════════════

-- | ToGVal for TestPriority - maps tpName → name, tpDescription → description
instance ToGVal (Run SourcePos (Writer Text) Text) TestPriority where
  toGVal tp = dict
    [ ("name", toGVal tp.tpName)
    , ("description", toGVal tp.tpDescription)
    ]

-- | Context for skeleton templates (impl-skeleton, test-skeleton).
--
-- Template variables:
--   {{ moduleName }} - e.g., "Data.Stack"
--   {{ typeName }} - e.g., "Stack"
--   {{ dataType }} - full type definition
--   {{ signatures }} - list of FunctionSig with .name, .signature, .description
--   {{ testPriorities }} - list of TestPriority with .name, .description
--   {{ imports }} - list of additional import statements
data SkeletonContext = SkeletonContext
  { moduleName :: Text
  , typeName :: Text
    -- ^ Type constructor name extracted from dataType (e.g., "Stack")
  , dataType :: Text
  , signatures :: [FunctionSig]
  , testPriorities :: [TestPriority]
  , imports :: [Text]
    -- ^ Additional imports requested by the types agent
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) SkeletonContext where
  toGVal SkeletonContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("typeName", toGVal typeName)
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    , ("testPriorities", list $ map toGVal testPriorities)
    , ("imports", list $ map toGVal imports)
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- v3 STUBS WORKFLOW CONTEXTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the stubs template (v3 workflow).
--
-- The stubs agent writes actual .hs files and returns semantic descriptions.
--
-- Template variables:
--   {{ moduleName }} - e.g., "UrlShortener"
--   {{ modulePath }} - e.g., "UrlShortener" (for file path: src/{{ modulePath }}.hs)
--   {{ description }} - natural language description
--   {{ acceptanceCriteria }} - list of acceptance criteria strings
data StubsContext = StubsContext
  { moduleName :: Text
    -- ^ Module name for the data structure.
  , modulePath :: Text
    -- ^ Module path (moduleName with dots replaced by slashes)
  , description :: Text
    -- ^ Natural language description.
  , acceptanceCriteria :: [Text]
    -- ^ High-level acceptance criteria to inform test priorities.
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) StubsContext where
  toGVal ctx = dict
    [ ("moduleName", toGVal ctx.moduleName)
    , ("modulePath", toGVal ctx.modulePath)
    , ("description", toGVal ctx.description)
    , ("acceptanceCriteria", list $ map toGVal ctx.acceptanceCriteria)
    ]


-- | Context for the tests template (v3 workflow).
--
-- Driven by semantic descriptions from the stubs agent.
--
-- Template variables:
--   {{ moduleName }} - module name
--   {{ dataType }} - data type definitions (for reference)
--   {{ semantics }} - list of FunctionSemantics with .name, .signature, .behavior, .examples
data TestsContextV3 = TestsContextV3
  { moduleName :: Text
  , dataType :: Text
  , semantics :: [FunctionSemantics]
  }
  deriving (Show, Eq, Generic)

-- | ToGVal for FunctionExample - exposes input, expected to templates
instance ToGVal (Run SourcePos (Writer Text) Text) FunctionExample where
  toGVal ex = dict
    [ ("input", toGVal ex.feInput)
    , ("expected", toGVal ex.feExpected)
    ]

-- | ToGVal for FunctionSemantics - exposes name, signature, behavior, examples to templates
instance ToGVal (Run SourcePos (Writer Text) Text) FunctionSemantics where
  toGVal sem = dict
    [ ("name", toGVal sem.fsmName)
    , ("signature", toGVal sem.fsmSignature)
    , ("behavior", toGVal sem.fsmBehavior)
    , ("examples", list $ map toGVal sem.fsmExamples)
    ]

instance ToGVal (Run SourcePos (Writer Text) Text) TestsContextV3 where
  toGVal TestsContextV3{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataType", toGVal dataType)
    , ("semantics", list $ map toGVal semantics)
    ]
