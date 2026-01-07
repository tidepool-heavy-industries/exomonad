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
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)


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
--   {{ signatures }} - list of type signatures
--   {{ moduleHeader }} - module header with exports
data TestsContext = TestsContext
  { moduleName :: Text
  , dataType :: Text
  , signatures :: [Text]
  , moduleHeader :: Text
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) TestsContext where
  toGVal TestsContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    , ("moduleHeader", toGVal moduleHeader)
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL TEMPLATE CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for the impl template.
--
-- Template variables:
--   {{ moduleName }} - module name
--   {{ dataType }} - data type definition
--   {{ signatures }} - list of type signatures
--   {{ moduleHeader }} - module header with exports
data ImplContext = ImplContext
  { moduleName :: Text
  , dataType :: Text
  , signatures :: [Text]
  , moduleHeader :: Text
  }
  deriving (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) ImplContext where
  toGVal ImplContext{..} = dict
    [ ("moduleName", toGVal moduleName)
    , ("dataType", toGVal dataType)
    , ("signatures", list $ map toGVal signatures)
    , ("moduleHeader", toGVal moduleHeader)
    ]
