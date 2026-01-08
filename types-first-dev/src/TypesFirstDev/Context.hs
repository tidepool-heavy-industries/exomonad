{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , SignatureInfo(..)
  , TestPriority(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import TypesFirstDev.Types (FunctionSig(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the types template.
--
-- Template variables:
--   {{ moduleName }} - e.g., "Data.Stack"
--   {{ description }} - natural language description
data TypesContext = TypesContext
  { moduleName :: Text
    -- ^ Module name for the data structure.
  , description :: Text
    -- ^ Natural language description.
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) TypesContext where
  toGVal ctx = dict
    [ ("moduleName", toGVal ctx.moduleName)
    , ("description", toGVal ctx.description)
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
data TestsContext = TestsContext
  { moduleName :: Text
  , dataType :: Text
  , signatures :: [FunctionSig]
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
-- SKELETON TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Information about a type signature.
--
-- Template variables:
--   {{ sig.name }} - function name
--   {{ sig.type }} - type signature
--   {{ sig.doc }} - documentation string
data SignatureInfo = SignatureInfo
  { name :: Text
  , sigType :: Text  -- ^ Called "type" in template
  , doc :: Text
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) SignatureInfo where
  toGVal SignatureInfo{..} = dict
    [ ("name", toGVal name)
    , ("type", toGVal sigType)
    , ("signature", toGVal sigType)  -- Alias for template compatibility
    , ("doc", toGVal doc)
    , ("description", toGVal doc)  -- Alias for template compatibility
    ]

-- | Test priority information.
--
-- Template variables:
--   {{ priority.name }} - test name
--   {{ priority.description }} - why this test matters
data TestPriority = TestPriority
  { name :: Text
  , description :: Text
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) TestPriority where
  toGVal TestPriority{..} = dict
    [ ("name", toGVal name)
    , ("description", toGVal description)
    ]

-- | Context for skeleton templates (impl-skeleton, test-skeleton).
--
-- Template variables:
--   {{ moduleName }} - e.g., "Data.Stack"
--   {{ dataTypeName }} - e.g., "Stack"
--   {{ dataType }} - full type definition
--   {{ signatures }} - list of SignatureInfo
--   {{ testPriorities }} - list of TestPriority
data SkeletonContext = SkeletonContext
  { moduleName :: Text
  , dataTypeName :: Text
  , typeName :: Text  -- ^ Alias of dataTypeName for template compatibility
  , dataType :: Text
  , signatures :: [SignatureInfo]
  , testPriorities :: [TestPriority]
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) SkeletonContext where
  toGVal SkeletonContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataTypeName", toGVal dataTypeName)
    , ("typeName", toGVal dataTypeName)  -- Alias for template compatibility
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    , ("testPriorities", list $ map toGVal testPriorities)
    ]
