{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Compile-time type-checked templates for Ginger.
--
-- This module provides Template Haskell splices that verify at compile time
-- that template variable accesses match the fields of a Haskell data type.
--
-- = Core Guarantee
--
-- If a template compiles successfully with 'typedTemplateFile', all field
-- accesses in the template are guaranteed to succeed at runtime.
--
-- = Usage
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- data User = User
--   { userName :: Text
--   , userEmail :: Text
--   }
--
-- userTemplate :: TypedTemplate User SourcePos
-- userTemplate = $(typedTemplateFile ''User "templates/user.html")
--
-- renderUser :: User -> Text
-- renderUser user = runTypedTemplate user userTemplate
-- @
--
-- = Supported Patterns
--
-- * Static field access: @{{ user.name }}@, @{{ user["name"] }}@
-- * Nested access: @{{ user.profile.bio }}@
-- * Sum types: field must exist in ALL constructors
-- * Recursive types: supported naturally
--
-- = Rejected Patterns (Compile Error)
--
-- * Dynamic key access: @{{ user[varName] }}@
-- * Fields not in all constructors of sum types
-- * Polymorphic types
-- * Unknown types
--
-- = Dependency Tracking
--
-- Templates loaded with 'typedTemplateFile' track all file dependencies
-- (the root template and all transitive includes). This enables:
--
-- * File watching for hot-reload systems
-- * Build system integration
-- * Template introspection
--
-- @
-- -- Get all template dependencies at runtime:
-- absolutePaths myTemplate  -- [\"/full/path/main.html\", \"/full/path/header.html\"]
-- relativePaths myTemplate  -- [\"templates/main.html\", \"header.html\"]
-- @
--
module Text.Ginger.TH
  ( -- * Template Haskell splices
    typedTemplateFile
  , typedTemplate
    -- * QuasiQuoter
  , jinja
    -- * Type-safe rendering
  , runTypedTemplate
  , runTypedTemplateM
    -- * Types
  , TypedTemplate(..)
    -- * Dependency tracking
    -- | Templates track their file dependencies as a tree.
    -- Use these to implement file watching, hot-reload, or build integration.
  , TemplateDependency(..)
  , DepRelation(..)
  , DepLocation(..)
  , flattenDeps
  , absolutePaths
  , relativePaths
    -- * Context type metadata
  , TemplateContextInfo(..)
  ) where

import Control.Monad (when)
import Data.IORef
import Data.List (nub)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import Text.Parsec.Pos (SourcePos, sourceName)
import Data.Monoid ((<>))
import Control.Monad.Writer (Writer)
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath (takeDirectory, (</>))

import Text.Ginger.AST (Template(..), Statement(..), Block(..), Macro(..), CatchBlock(..), Annotated(..))
import Text.Ginger.GVal (ToGVal, GVal)
import Text.Ginger.Parse (parseGinger', ParserError(..), ParserOptions(..), mkParserOptions, sourceLine, sourceColumn)
import Text.Ginger.Run (easyRender, easyRenderM)
import Text.Ginger.Run.Type (Run, ContextEncodable, RuntimeError)

import Text.Ginger.TH.Types
import Text.Ginger.TH.Builtins (isBuiltin)
import Text.Ginger.TH.Extract (extractFromTemplate)
import Text.Ginger.TH.QuasiQuote (jinja, jinjaRender)
import Text.Ginger.TH.Schema (generateSchema, SchemaRegistry)
import Text.Ginger.TH.Validate (validatePaths, formatValidationErrorsWithSource)

-- | Load and type-check a template from a file at compile time.
--
-- The first argument is the name of the context type (use @''TypeName@ syntax).
-- The second argument is the path to the template file.
--
-- @
-- myTemplate :: TypedTemplate MyContext SourcePos
-- myTemplate = $(typedTemplateFile ''MyContext "templates/my-template.html")
-- @
--
-- This will:
--
-- 1. Read the template file at compile time
-- 2. Parse the template (including any @{% include %}@ directives)
-- 3. Extract all variable accesses from the template and included templates
-- 4. Generate a schema from the Haskell type
-- 5. Validate that all accesses match the schema
-- 6. Fail compilation with clear error messages if validation fails
-- 7. Register all included files as dependencies (recompile if they change)
--
typedTemplateFile :: Name -> FilePath -> Q Exp
typedTemplateFile typeName templatePath = do
  -- Register the template file as a dependency for recompilation
  addDependentFile templatePath

  -- Read template source at compile time
  src <- runIO $ readFile templatePath

  -- Create a resolver that reads files
  let resolver path = do
        exists <- doesFileExist path
        if exists then Just <$> readFile path else return Nothing

  -- Parse the template
  let opts = (mkParserOptions resolver) { poSourceName = Just templatePath }
  parseResult <- runIO $ parseGinger' opts src

  template <- case parseResult of
    Left err -> fail $ formatParserErrorWithSource (Just templatePath) src err
    Right tpl -> return tpl

  -- Generate schema from the Haskell type
  (schema, registry) <- generateSchema typeName

  -- Extract variable accesses from template
  let accesses = extractFromTemplate template

  -- Filter out builtins
  let userAccesses = filter (not . isBuiltin . apRoot) accesses

  -- Validate accesses against schema
  let errors = validatePaths registry schema userAccesses

  -- Report errors or generate code
  when (not $ null errors) $
    fail $ formatValidationErrorsWithSource src errors

  -- Extract dependency tree from AST
  depTree <- runIO $ extractDependenciesFromAST templatePath template

  -- Register all files with GHC for recompilation
  mapM_ (addDependentFile . depAbsolutePath) (flattenDeps depTree)

  -- Get context type metadata
  let simpleName = nameBase typeName
  let moduleName = nameModule typeName
  let fullyQualified = case moduleName of
        Just m -> m ++ "." ++ simpleName
        Nothing -> simpleName
  let contextInfo = TemplateContextInfo simpleName moduleName fullyQualified

  -- Get accessed fields (filter builtins, format as "root.path.to.field")
  let accessedFields = nub $ map formatAccessPath userAccesses

  -- Generate code (use lift + AppE pattern to avoid TH splice issues)
  depTreeExp <- lift depTree
  contextInfoExp <- lift contextInfo
  fieldsExp <- lift accessedFields
  let srcLit = litE (stringL src)
  let pathLit = [| Just templatePath |]
  templateExp <- [| unsafeParseTemplateRaw $pathLit $srcLit |]

  return $ foldl AppE (ConE 'TypedTemplate)
    [templateExp, depTreeExp, contextInfoExp, fieldsExp]

-- | Type-check an inline template string at compile time.
--
-- Note: Inline templates do not support @{% include %}@ directives.
-- Use 'typedTemplateFile' for templates with includes.
--
-- @
-- myTemplate :: TypedTemplate MyContext SourcePos
-- myTemplate = $(typedTemplate ''MyContext "Hello, {{ userName }}!")
-- @
--
typedTemplate :: Name -> String -> Q Exp
typedTemplate typeName src = do
  -- Parse the template
  let opts = (mkParserOptions nullResolver) { poSourceName = Nothing }
  parseResult <- runIO $ parseGinger' opts src

  template <- case parseResult of
    Left err -> fail $ formatParserErrorWithSource Nothing src err
    Right tpl -> return tpl

  -- Generate schema from the Haskell type
  (schema, registry) <- generateSchema typeName

  -- Extract variable accesses from template
  let accesses = extractFromTemplate template

  -- Filter out builtins
  let userAccesses = filter (not . isBuiltin . apRoot) accesses

  -- Validate accesses against schema
  let errors = validatePaths registry schema userAccesses

  -- Report errors or generate code
  when (not $ null errors) $
    fail $ formatValidationErrorsWithSource src errors

  -- Get context type metadata
  let simpleName = nameBase typeName
  let moduleName = nameModule typeName
  let fullyQualified = case moduleName of
        Just m -> m ++ "." ++ simpleName
        Nothing -> simpleName
  let contextInfo = TemplateContextInfo simpleName moduleName fullyQualified

  -- Get accessed fields
  let accessedFields = nub $ map formatAccessPath userAccesses

  -- Inline templates have an empty dependency tree (no file, no children)
  let emptyDepTree = TemplateDependency
        { depAbsolutePath = "<inline>"
        , depRelativePath = "<inline>"
        , depRelation = Nothing
        , depIncludeLocation = Nothing
        , depChildren = []
        }
  depTreeExp <- lift emptyDepTree
  contextInfoExp <- lift contextInfo
  fieldsExp <- lift accessedFields
  let srcLit = litE (stringL src)
  templateExp <- [| unsafeParseTemplateRaw Nothing $srcLit |]

  return $ foldl AppE (ConE 'TypedTemplate)
    [templateExp, depTreeExp, contextInfoExp, fieldsExp]

-- | Null include resolver (no includes supported for inline templates).
nullResolver :: Monad m => String -> m (Maybe String)
nullResolver _ = return Nothing

-- | Format an AccessPath as "root.path.to.field"
formatAccessPath :: AccessPath -> String
formatAccessPath ap = Text.unpack (apRoot ap) ++
  concatMap formatSegment (apPath ap)
  where
    formatSegment (StaticKey k) = "." ++ Text.unpack k
    formatSegment DynamicKey = "[*]"

-- | Extract dependency tree from parsed template AST.
-- Returns the root node with children representing includes/extends.
extractDependenciesFromAST :: FilePath -> Template SourcePos -> IO TemplateDependency
extractDependenciesFromAST rootPath tpl = do
  rootAbs <- canonicalizePath rootPath

  -- Get extends dependencies (from templateParent)
  extendsDeps <- case templateParent tpl of
    Nothing -> return []
    Just parentTpl -> extractExtends parentTpl

  -- Get include dependencies (from statements)
  includeDeps <- walkStatement (templateBody tpl)

  -- Also walk blocks (they may contain includes)
  blockDeps <- concat <$> mapM (walkStatement . blockBody)
                                (HashMap.elems $ templateBlocks tpl)

  return TemplateDependency
    { depAbsolutePath = rootAbs
    , depRelativePath = rootPath
    , depRelation = Nothing
    , depIncludeLocation = Nothing
    , depChildren = extendsDeps ++ includeDeps ++ blockDeps
    }

-- | Extract extends chain from templateParent
extractExtends :: Template SourcePos -> IO [TemplateDependency]
extractExtends parentTpl = do
  let parentPath = getSourceFile (templateBody parentTpl)
  -- Check for "<<unknown>>" fallback
  if parentPath == "<<unknown>>" || null parentPath
    then return []  -- Can't track this dependency
    else do
      parentAbs <- canonicalizePath parentPath
      let pos = annotation (templateBody parentTpl)

      -- Recursively get this parent's children
      childTree <- extractDependenciesFromAST parentPath parentTpl

      return [TemplateDependency
        { depAbsolutePath = parentAbs
        , depRelativePath = parentPath
        , depRelation = Just DepExtended
        , depIncludeLocation = Just $ posToDepLocation pos
        , depChildren = depChildren childTree  -- inherit the children
        }]

-- | Walk all statement types to find includes (returns child nodes)
walkStatement :: Statement SourcePos -> IO [TemplateDependency]
walkStatement stmt = case stmt of
  PreprocessedIncludeS pos includedTpl -> do
    let childPath = getSourceFile (templateBody includedTpl)
    if childPath == "<<unknown>>" || null childPath
      then return []
      else do
        childAbs <- canonicalizePath childPath

        -- Recursively get this include's children
        childTree <- extractDependenciesFromAST childPath includedTpl

        return [TemplateDependency
          { depAbsolutePath = childAbs
          , depRelativePath = childPath
          , depRelation = Just DepIncluded
          , depIncludeLocation = Just $ posToDepLocation pos
          , depChildren = depChildren childTree  -- inherit the children
          }]

  -- Statements with nested statements
  MultiS _ stmts -> concat <$> mapM walkStatement stmts
  ScopedS _ body -> walkStatement body
  IndentS _ _ body -> walkStatement body
  IfS _ _ thenBranch elseBranch -> do
    t <- walkStatement thenBranch
    e <- walkStatement elseBranch
    return (t ++ e)
  SwitchS _ _ cases defaultCase -> do
    caseDeps <- concat <$> mapM (walkStatement . snd) cases
    defaultDeps <- walkStatement defaultCase
    return (caseDeps ++ defaultDeps)
  ForS _ _ _ _ body -> walkStatement body
  DefMacroS _ _ macro -> walkStatement (macroBody macro)
  TryCatchS _ tryBody catches finallyBody -> do
    tryDeps <- walkStatement tryBody
    catchDeps <- concat <$> mapM (walkStatement . catchBody) catches
    finallyDeps <- walkStatement finallyBody
    return (tryDeps ++ catchDeps ++ finallyDeps)

  -- Leaf statements (no nested content)
  _ -> return []

-- | Extract source file name from a statement's SourcePos
getSourceFile :: Statement SourcePos -> FilePath
getSourceFile stmt = sourceName (annotation stmt)

-- | Convert SourcePos to DepLocation
posToDepLocation :: SourcePos -> DepLocation
posToDepLocation pos = DepLocation
  { depLocFile = sourceName pos
  , depLocLine = sourceLine pos
  , depLocColumn = sourceColumn pos
  }

-- | Format a parser error with source context (Rust-style).
formatParserErrorWithSource :: Maybe FilePath -> String -> ParserError -> String
formatParserErrorWithSource mPath src err =
  unlines $ catMaybes
    [ Just $ "error: Template parse error" ++ pathSuffix
    , locationLine
    , Just "   |"
    , sourceLine'
    , caretLine
    , Just "   |"
    , Just $ "   = " ++ peErrorMessage err
    ]
  where
    pathSuffix = maybe "" (" in " ++) mPath
    sourceLines = lines src

    locationLine = do
      pos <- peSourcePosition err
      return $ "  --> " ++ maybe "<unknown>" id mPath ++ ":" ++
               show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)

    sourceLine' = do
      pos <- peSourcePosition err
      let lineNum = sourceLine pos
      line <- listToMaybe $ drop (lineNum - 1) sourceLines
      return $ padLineNum lineNum ++ " | " ++ line

    caretLine = do
      pos <- peSourcePosition err
      let col = sourceColumn pos
      return $ "   | " ++ replicate (col - 1) ' ' ++ "^"

    padLineNum n = let s = show n in replicate (3 - length s) ' ' ++ s

-- | Parse a template at runtime. Used internally by generated code.
-- This should not fail since we already validated at compile time.
-- Returns the raw Template, which is then wrapped in TypedTemplate by the caller.
unsafeParseTemplateRaw :: Maybe String -> String -> Template SourcePos
unsafeParseTemplateRaw mPath src =
  let opts = (mkParserOptions nullResolver) { poSourceName = mPath }
      result = runIdentity $ parseGinger' opts src
  in case result of
       Left err -> error $ "BUG: Template that passed compile-time validation failed to parse at runtime: " ++ peErrorMessage err
       Right tpl -> tpl
  where
    runIdentity :: Identity a -> a
    runIdentity (Identity a) = a

newtype Identity a = Identity { unIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

-- | Render a typed template with the correct context type.
-- Pure version that returns the rendered output directly.
--
-- @
-- let html = runTypedTemplate userData userTemplate
-- @
--
runTypedTemplate :: ( ContextEncodable h
                    , Monoid h
                    , ToGVal (Run SourcePos (Writer h) h) a
                    , ToGVal (Run SourcePos (Writer h) h) h
                    , ToGVal (Run SourcePos (Writer h) h) SourcePos
                    )
                 => a
                 -> TypedTemplate a SourcePos
                 -> h
runTypedTemplate context typedTpl =
  easyRender context (unTypedTemplate typedTpl)

-- | Render a typed template in a monadic context.
--
-- @
-- runTypedTemplateM putStr userData userTemplate
-- @
--
runTypedTemplateM :: ( Monad m
                     , ContextEncodable h
                     , Monoid h
                     , ToGVal (Run SourcePos m h) a
                     , ToGVal (Run SourcePos m h) h
                     , ToGVal (Run SourcePos m h) SourcePos
                     )
                  => (h -> m ())
                  -> a
                  -> TypedTemplate a SourcePos
                  -> m (Either (RuntimeError SourcePos) (GVal (Run SourcePos m h)))
runTypedTemplateM emit context typedTpl =
  easyRenderM emit context (unTypedTemplate typedTpl)
