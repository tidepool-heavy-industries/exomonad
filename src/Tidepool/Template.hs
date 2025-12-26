{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Template types and TH for mustache validation
module Tidepool.Template
  ( -- * Template Type
    Template(..)
  , Partial
  
    -- * Template Construction
  , mkTemplate
  , mkPartial
  
    -- * Rendering
  , render
  , renderPartial
  
    -- * Schema
  , Schema(..)
  , ToolSchemas(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Value)

-- | A template is a typed contract between input and rendered output
data Template input output tools = Template
  { templateRender :: input -> Text
  , templateOutputSchema :: Schema output
  , templateTools :: ToolSchemas tools
  }

-- | A partial is a reusable template block
newtype Partial a = Partial { runPartial :: a -> Text }

-- | JSON schema with descriptions for structured output
data Schema a = Schema
  { schemaJSON :: Value
  , schemaDescription :: Text
  }
  deriving (Show, Eq, Generic)

-- | Placeholder for tool schema list
data ToolSchemas (tools :: [*]) = ToolSchemas
  deriving (Show, Eq, Generic)

-- | Render a template with input
render :: Template input output tools -> input -> Text
render t = t.templateRender

-- | Render a partial with input
renderPartial :: Partial a -> a -> Text
renderPartial p = p.runPartial

-- ══════════════════════════════════════════════════════════════
-- TEMPLATE HASKELL
-- ══════════════════════════════════════════════════════════════

-- | Compile-time template validation
--
-- Usage:
-- @
-- mkTemplate ''MyInput "templates/my_template.mustache"
-- @
--
-- This should:
-- 1. Parse mustache file at compile time
-- 2. Extract all variable references
-- 3. Validate paths against input type via Generics
-- 4. Generate: renderMyInput :: MyInput -> Text
mkTemplate :: Name -> FilePath -> Q [Dec]
mkTemplate _inputType _templatePath = error "TODO: mkTemplate TH splice - parse mustache, validate fields against type, generate render function"

-- | Create a typed partial from inline mustache
mkPartial :: Name -> String -> Q Exp
mkPartial _inputType _templateStr = error "TODO: mkPartial TH splice - parse inline mustache, validate, return Partial value"

-- | Mustache quasi-quoter (for inline templates)
mustache :: QuasiQuoter
mustache = QuasiQuoter
  { quoteExp = \s -> error "TODO: mustache quasiquoter - parse and validate inline template"
  , quotePat = \_ -> error "mustache quasiquoter does not support patterns"
  , quoteType = \_ -> error "mustache quasiquoter does not support types"
  , quoteDec = \_ -> error "mustache quasiquoter does not support declarations"
  }
