{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | QuasiQuoter for Ginger templates with variable capture.
--
-- Templates are parsed at compile time (syntax errors = compile errors).
-- Variable references in the template are captured from Haskell scope.
--
-- = Usage
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
-- import Text.Ginger.TH (jinja)
--
-- greeting :: Text -> Text
-- greeting name = [jinja|Hello, {{ name }}!|]
-- @
--
module Text.Ginger.TH.QuasiQuote
  ( jinja
  , jinjaRender  -- exported for generated code
  ) where

import Control.Monad.Writer (Writer)
import Data.Default (def)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec.Pos (SourcePos)

import Text.Ginger.AST (Template)
import Text.Ginger.GVal (GVal(GVal), ToGVal, toGVal, asLookup)
import Text.Ginger.Parse (parseGinger', ParserError(..), mkParserOptions, ParserOptions(..), sourceLine, sourceColumn)
import Text.Ginger.Run (easyRender)
import Text.Ginger.Run.Type (Run)
import Text.Ginger.TH.Extract (extractFromTemplate)
import Text.Ginger.TH.Types (AccessPath(..), apRoot)
import Text.Ginger.TH.Builtins (isBuiltin)

-- | Template QuasiQuoter with variable capture.
--
-- Variables referenced in the template are captured from Haskell scope
-- and converted via 'ToGVal'. Returns rendered 'Text'.
--
-- @
-- greeting name = [jinja|Hello, {{ name }}!|]
-- greeting "world"  -- "Hello, world!"
-- @
--
jinja :: QuasiQuoter
jinja = QuasiQuoter
  { quoteExp = parseAndEmbed
  , quotePat = unsupported "pattern"
  , quoteType = unsupported "type"
  , quoteDec = unsupported "declaration"
  }

unsupported :: String -> String -> Q a
unsupported ctx _ = fail $ "jinja QuasiQuoter not supported in " ++ ctx ++ " context"

parseAndEmbed :: String -> Q Exp
parseAndEmbed src = do
  loc <- location
  let sourceName = loc_filename loc ++ ":" ++ show (fst $ loc_start loc)
  let opts = (mkParserOptions nullResolver) { poSourceName = Just sourceName }

  parseResult <- runIO $ parseGinger' opts src

  case parseResult of
    Left err -> fail $ formatParseError sourceName src err
    Right tpl -> do
      -- Extract variable accesses from template
      let accesses = extractFromTemplate tpl
      -- Filter out builtins and get unique root variable names
      let varNames = Set.toList $ Set.fromList
            [ Text.unpack (apRoot ap)
            | ap <- accesses
            , not (isBuiltin (apRoot ap))
            ]

      -- Generate a lookup function as a case expression
      -- jinjaRender (\k -> case k of "name" -> Just (toGVal name); ...; _ -> Nothing) (unsafeParseTemplate src)
      keyName <- newName "k"
      let lookupBody = caseE (varE keyName) (map (mkMatch keyName) varNames ++ [defaultMatch])
      let lookupFn = lamE [varP keyName] lookupBody
      [| jinjaRender $lookupFn (unsafeParseTemplate src) |]

-- | Generate a case match: "varName" -> Just (toGVal varName)
mkMatch :: Name -> String -> Q Match
mkMatch _ varName = do
  let nameE = varE (mkName varName)
  match (litP (stringL varName))
        (normalB [| Just (toGVal $nameE) |])
        []

-- | Default case: _ -> Nothing
defaultMatch :: Q Match
defaultMatch = match wildP (normalB [| Nothing |]) []

-- | Type alias for the GVal monad used in rendering
type M = Run SourcePos (Writer Text) Text

-- | Render a template with a lookup function for context.
-- This helper is used by generated QuasiQuoter code.
jinjaRender :: (Text -> Maybe (GVal M)) -> Template SourcePos -> Text
jinjaRender lookupFn tpl = easyRender ctx tpl
  where
    ctx :: GVal M
    ctx = def { asLookup = Just lookupFn }

-- | Null resolver for inline templates (no includes).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | Parse template at runtime. Used by generated code.
-- Should not fail since we validated at compile time.
unsafeParseTemplate :: String -> Template SourcePos
unsafeParseTemplate src =
  let opts = (mkParserOptions nullResolver) { poSourceName = Nothing }
      result = runIdentity $ parseGinger' opts src
  in case result of
       Left err -> error $ "BUG: Template that passed compile-time validation failed to parse: " ++ peErrorMessage err
       Right tpl -> tpl
  where
    runIdentity :: Identity a -> a
    runIdentity (Identity a) = a

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

-- | Format parse error with source context.
formatParseError :: String -> String -> ParserError -> String
formatParseError sourceName src err =
  unlines
    [ "error: Template parse error in " ++ sourceName
    , locationInfo
    , "   |"
    , sourceLineStr
    , caretLine
    , "   |"
    , "   = " ++ peErrorMessage err
    ]
  where
    sourceLines = lines src

    locationInfo = case peSourcePosition err of
      Just pos -> "  --> " ++ sourceName ++ ":" ++
                  show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
      Nothing -> "  --> " ++ sourceName

    sourceLineStr = case peSourcePosition err of
      Just pos ->
        let lineNum = sourceLine pos
            mLine = if lineNum > 0 && lineNum <= length sourceLines
                    then Just (sourceLines !! (lineNum - 1))
                    else Nothing
        in case mLine of
             Just line -> padLineNum lineNum ++ " | " ++ line
             Nothing -> "   |"
      Nothing -> "   |"

    caretLine = case peSourcePosition err of
      Just pos -> "   | " ++ replicate (sourceColumn pos - 1) ' ' ++ "^"
      Nothing -> "   |"

    padLineNum n = let s = show n in replicate (3 - length s) ' ' ++ s
