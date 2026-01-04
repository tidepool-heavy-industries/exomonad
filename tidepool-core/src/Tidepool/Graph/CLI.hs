{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI derivation for tidepool graphs.
--
-- This module enables wrapping any tidepool graph as a CLI tool where:
--
-- * Entry type → CLI arguments (derived from Haddock via TH)
-- * Exit type → CLI output (JSON or text via @--format@ flag)
--
-- = Usage
--
-- 1. Define your input type in a separate module with Haddock docs:
--
-- @
-- -- In Types.hs
-- {-\# LANGUAGE FieldSelectors \#-}
--
-- data MyInput = MyInput
--   { inputFile :: FilePath
--     -- ^ Path to the input file to process
--   , verbose :: Bool
--     -- ^ Enable verbose output
--   }
-- @
--
-- 2. Derive the parser and run:
--
-- @
-- -- In Main.hs
-- import Types (MyInput(..))
--
-- main :: IO ()
-- main = runGraphCLIWith
--   "Process files with my graph"
--   $(deriveCLIParser ''MyInput)
--   runMyGraph
-- @
--
-- = Requirements
--
-- Same as 'Tidepool.Schema.deriveJSONSchema':
--
-- 1. Input type must be in a separate, already-compiled module (TH staging)
-- 2. That module must have @{-\# LANGUAGE FieldSelectors \#-}@
-- 3. All fields must have @-- ^@ Haddock documentation
module Tidepool.Graph.CLI
  ( -- * GraphCLI Typeclass
    GraphCLI(..)

    -- * Type Families for Entry/Exit Extraction
  , GraphEntryType
  , GraphExitType
  , ValidEntry
  , ValidExit

    -- * TH Parser Derivation
  , deriveCLIParser

    -- * Output Formatting
  , OutputFormat(..)
  , outputFormatParser
  , formatOutput

    -- * CLI Runner
  , runGraphCLIWith

    -- * Helpers (exported for testing)
  , toKebabCase
  ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Text qualified as Aeson
import Data.Char (isUpper, toLower)
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic(..), M1(..), K1(..), (:*:))
import GHC.Generics qualified as G
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified as TH
import Options.Applicative

import Tidepool.Graph.Generic.Core (AsGraph, Entry, Exit)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPHCLI TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for graphs that can be run via CLI.
--
-- This is a minimal typeclass - just provides the description.
-- Parser derivation happens via TH ('deriveCLIParser'), and output
-- formatting is handled by 'Show' + 'ToJSON' constraints.
--
-- @
-- instance GraphCLI MyGraph where
--   cliDescription = "Process items for a user"
-- @
type GraphCLI :: (Type -> Type) -> Constraint
class GraphCLI graph where
  -- | Description shown in @--help@ output.
  --
  -- Default: @"Run graph"@
  cliDescription :: Text
  cliDescription = "Run graph"

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE FAMILIES FOR ENTRY/EXIT EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Entry type from a graph record's Generic representation.
--
-- @
-- GraphEntryType MyGraph = Input  -- if entry :: mode :- Entry Input
-- @
--
-- Produces a helpful type error if no Entry is found.
type GraphEntryType :: (Type -> Type) -> Type
type family GraphEntryType graph where
  GraphEntryType graph = ExtractEntry (Rep (graph AsGraph))

-- | Extract Exit type from a graph record's Generic representation.
--
-- @
-- GraphExitType MyGraph = Output  -- if exit :: mode :- Exit Output
-- @
--
-- Produces a helpful type error if no Exit is found.
type GraphExitType :: (Type -> Type) -> Type
type family GraphExitType graph where
  GraphExitType graph = ExtractExit (Rep (graph AsGraph))

-- | Extract Entry type from Generic representation.
--
-- Traverses the representation looking for @Entry a@ field types.
-- Returns 'EntryNotFound' sentinel if not found.
type ExtractEntry :: (Type -> Type) -> Type
type family ExtractEntry f where
  ExtractEntry (M1 G.D _ f) = ExtractEntry f
  ExtractEntry (M1 G.C _ f) = ExtractEntry f
  ExtractEntry (M1 G.S _ (K1 _ (Entry a))) = a
  ExtractEntry (M1 G.S _ _) = EntryNotFound
  ExtractEntry (l :*: r) = ChooseEntry (ExtractEntry l) (ExtractEntry r)
  ExtractEntry _ = EntryNotFound

-- | Extract Exit type from Generic representation.
--
-- Traverses the representation looking for @Exit a@ field types.
-- Returns 'ExitNotFound' sentinel if not found.
type ExtractExit :: (Type -> Type) -> Type
type family ExtractExit f where
  ExtractExit (M1 G.D _ f) = ExtractExit f
  ExtractExit (M1 G.C _ f) = ExtractExit f
  ExtractExit (M1 G.S _ (K1 _ (Exit a))) = a
  ExtractExit (M1 G.S _ _) = ExitNotFound
  ExtractExit (l :*: r) = ChooseExit (ExtractExit l) (ExtractExit r)
  ExtractExit _ = ExitNotFound

-- | Sentinel for missing Entry - will produce a type error when used.
data EntryNotFound

-- | Sentinel for missing Exit - will produce a type error when used.
data ExitNotFound

-- | Choose between Entry candidates (take first non-sentinel).
type ChooseEntry :: Type -> Type -> Type
type family ChooseEntry l r where
  ChooseEntry EntryNotFound r = r
  ChooseEntry l _ = l

-- | Choose between Exit candidates (take first non-sentinel).
type ChooseExit :: Type -> Type -> Type
type family ChooseExit l r where
  ChooseExit ExitNotFound r = r
  ChooseExit l _ = l

-- | Constraint that validates Entry type was found.
-- Produces a helpful type error if 'EntryNotFound' sentinel is present.
type ValidEntry :: Type -> Constraint
type family ValidEntry t where
  ValidEntry EntryNotFound = TypeError
    ('Text "GraphCLI: Could not find Entry in graph."
     ':$$: 'Text ""
     ':$$: 'Text "Ensure your graph record has an Entry field:"
     ':$$: 'Text "  data MyGraph mode = MyGraph"
     ':$$: 'Text "    { entry :: mode :- Entry YourInputType"
     ':$$: 'Text "    , ..."
     ':$$: 'Text "    }")
  ValidEntry _ = ()

-- | Constraint that validates Exit type was found.
-- Produces a helpful type error if 'ExitNotFound' sentinel is present.
type ValidExit :: Type -> Constraint
type family ValidExit t where
  ValidExit ExitNotFound = TypeError
    ('Text "GraphCLI: Could not find Exit in graph."
     ':$$: 'Text ""
     ':$$: 'Text "Ensure your graph record has an Exit field:"
     ':$$: 'Text "  data MyGraph mode = MyGraph"
     ':$$: 'Text "    { ..."
     ':$$: 'Text "    , exit :: mode :- Exit YourOutputType"
     ':$$: 'Text "    }")
  ValidExit _ = ()

-- ════════════════════════════════════════════════════════════════════════════
-- TH PARSER DERIVATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Derive an optparse-applicative 'Parser' from a record type.
--
-- Uses Haddock comments as help text for each flag.
--
-- == Requirements
--
-- 1. Type must be in a separate, already-compiled module (TH staging)
-- 2. That module must have @{-\# LANGUAGE FieldSelectors \#-}@
-- 3. All fields must have @-- ^@ Haddock documentation
--
-- == Field Type Mapping
--
-- @
-- Text/String  → strOption (--field-name TEXT)
-- Int/Integer  → option auto (--field-name INT)
-- Bool         → switch (--field-name)
-- Maybe a      → optional (parser for a)
-- [a]          → many (parser for a)
-- @
--
-- == Example
--
-- @
-- -- In Types.hs (separate module)
-- {-\# LANGUAGE FieldSelectors \#-}
--
-- data ProcessInput = ProcessInput
--   { inputPath :: FilePath
--     -- ^ Path to input file
--   , outputPath :: Maybe FilePath
--     -- ^ Optional output path (default: stdout)
--   , verbose :: Bool
--     -- ^ Enable verbose logging
--   }
--
-- -- In Main.hs
-- main = runGraphCLIWith "Process files" $(deriveCLIParser ''ProcessInput) processFile
-- @
--
-- == Sum Types
--
-- Sum types become subcommands:
--
-- @
-- data Command
--   = Process { file :: FilePath }
--     -- ^ Process a file
--   | Validate { file :: FilePath, strict :: Bool }
--     -- ^ Validate a file
--
-- -- Generates: mycli process --file FILE
-- --            mycli validate --file FILE [--strict]
-- @
deriveCLIParser :: Name -> Q Exp
deriveCLIParser typeName = do
  info <- reify typeName
  case info of
    -- Single constructor record → flat flags
    TyConI (DataD _ _ _ _ [RecC conName fields] _) ->
      deriveRecordParser typeName conName fields

    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) ->
      deriveRecordParser typeName conName fields

    -- Multiple constructors → subcommands
    TyConI (DataD _ _ _ _ cons _) | length cons > 1 ->
      deriveSubcommandParser typeName cons

    _ -> fail $ "deriveCLIParser: " ++ show typeName
             ++ " must be a record type or sum type with record constructors"

-- | Derive parser for a single-constructor record.
deriveRecordParser :: Name -> Name -> [VarBangType] -> Q Exp
deriveRecordParser typeName conName fields = do
  fieldParsers <- mapM (deriveFieldParser typeName) fields
  let con = conE conName
  -- Build: ConName <$> p1 <*> p2 <*> ...
  case fieldParsers of
    [] -> [| pure $con |]
    (p:ps) -> foldl (\acc fp -> [| $acc <*> $(pure fp) |])
                    [| $con <$> $(pure p) |]
                    ps

-- | Derive parser for a single field.
deriveFieldParser :: Name -> VarBangType -> Q Exp
deriveFieldParser typeName (fieldName, _, fieldType) = do
  mDoc <- getDoc (DeclDoc fieldName)
  helpText <- case mDoc of
    Just doc -> pure doc
    Nothing -> fail $ cliFieldError typeName fieldName fieldType

  let flagName = toKebabCase (nameBase fieldName)
  typeToParserExp typeName flagName helpText fieldType

-- | Convert Haskell type to optparse-applicative parser expression.
typeToParserExp :: Name -> String -> String -> TH.Type -> Q Exp
typeToParserExp typeName flag helpText = go
  where
    helpMod = [| help $(litE (stringL helpText)) |]

    go :: TH.Type -> Q Exp
    go = \case
      -- Text → strOption
      ConT n | nameBase n == "Text" ->
        [| T.pack <$> strOption (long flag <> metavar "TEXT" <> $helpMod) |]

      -- String → strOption
      ConT n | nameBase n == "String" ->
        [| strOption (long flag <> metavar "TEXT" <> $helpMod) |]

      -- FilePath (alias for String) → strOption
      ConT n | nameBase n == "FilePath" ->
        [| strOption (long flag <> metavar "PATH" <> $helpMod) |]

      -- Int → option auto
      ConT n | nameBase n == "Int" ->
        [| option auto (long flag <> metavar "INT" <> $helpMod) |]

      -- Integer → option auto
      ConT n | nameBase n == "Integer" ->
        [| option auto (long flag <> metavar "INT" <> $helpMod) |]

      -- Double → option auto
      ConT n | nameBase n == "Double" ->
        [| option auto (long flag <> metavar "NUM" <> $helpMod) |]

      -- Bool → switch
      ConT n | nameBase n == "Bool" ->
        [| switch (long flag <> $helpMod) |]

      -- Maybe a → optional
      AppT (ConT n) inner | nameBase n == "Maybe" -> do
        innerParser <- go inner
        [| optional $(pure innerParser) |]

      -- [a] → many
      AppT ListT inner -> do
        innerParser <- go inner
        [| many $(pure innerParser) |]

      -- Nested record → error
      ConT n -> do
        nInfo <- reify n
        case nInfo of
          TyConI (DataD _ _ _ _ [RecC _ _] _) ->
            fail $ nestedRecordError typeName flag n
          TyConI (NewtypeD _ _ _ _ (RecC _ _) _) ->
            fail $ nestedRecordError typeName flag n
          _ -> fail $ unsupportedTypeError flag n

      other ->
        fail $ "deriveCLIParser: Unsupported field type for --" ++ flag
            ++ ": " ++ pprint other

-- | Derive subcommand parser from sum type constructors.
deriveSubcommandParser :: Name -> [Con] -> Q Exp
deriveSubcommandParser typeName cons = do
  commands <- mapM (deriveCommand typeName) cons
  case commands of
    [] -> fail "deriveCLIParser: Sum type must have at least one constructor"
    [c] -> [| subparser $(pure c) |]
    (c:cs) -> do
      let combined = foldl (\acc cmd -> [| $acc <> $(pure cmd) |])
                           (pure c) cs
      [| subparser $combined |]

-- | Derive a single subcommand from a constructor.
deriveCommand :: Name -> Con -> Q Exp
deriveCommand typeName con = case con of
  RecC conName fields -> do
    parser <- deriveRecordParser typeName conName fields
    let cmdName = toKebabCase (nameBase conName)
    mDoc <- getDoc (DeclDoc conName)
    let desc = maybe ("Run " ++ nameBase conName) id mDoc
    [| command cmdName (info $(pure parser) (progDesc $(litE (stringL desc)))) |]

  NormalC conName [] -> do
    -- Nullary constructor → command with no arguments
    let cmdName = toKebabCase (nameBase conName)
        con' = conE conName
    mDoc <- getDoc (DeclDoc conName)
    let desc = maybe ("Run " ++ nameBase conName) id mDoc
    [| command cmdName (info (pure $con') (progDesc $(litE (stringL desc)))) |]

  _ -> fail $ "deriveCLIParser: Constructor " ++ show con
           ++ " must be a record or nullary constructor"

-- ════════════════════════════════════════════════════════════════════════════
-- OUTPUT FORMATTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Output format for CLI results.
data OutputFormat
  = FormatJSON  -- ^ Output as JSON
  | FormatText  -- ^ Output as text (via Show)
  deriving (Eq, Show)

-- | Parser for @--format@ flag.
--
-- @
-- --format json   → FormatJSON
-- --format text   → FormatText (default)
-- @
outputFormatParser :: Parser OutputFormat
outputFormatParser = option (eitherReader parseFormat)
  ( long "format"
  <> short 'f'
  <> metavar "FORMAT"
  <> value FormatText
  <> showDefaultWith formatName
  <> help "Output format: json or text"
  )
  where
    parseFormat :: String -> Either String OutputFormat
    parseFormat s = case map toLower s of
      "json" -> Right FormatJSON
      "text" -> Right FormatText
      _      -> Left $ "Unknown format '" ++ s ++ "'. Use 'json' or 'text'."

    formatName :: OutputFormat -> String
    formatName FormatJSON = "json"
    formatName FormatText = "text"

-- | Format output value according to the chosen format.
formatOutput :: (Show a, ToJSON a) => OutputFormat -> a -> Text
formatOutput FormatJSON = TL.toStrict . Aeson.encodeToLazyText
formatOutput FormatText = T.pack . show

-- ════════════════════════════════════════════════════════════════════════════
-- CLI RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph as a CLI application.
--
-- Combines:
--
-- * Input parser (from 'deriveCLIParser')
-- * @--format@ flag for output formatting
-- * @--help@ with description
--
-- @
-- main :: IO ()
-- main = runGraphCLIWith
--   "Process files with my graph"
--   $(deriveCLIParser ''MyInput)
--   runMyGraph
-- @
runGraphCLIWith
  :: (Show output, ToJSON output)
  => Text                       -- ^ Description for --help
  -> Parser input               -- ^ Input parser (use deriveCLIParser)
  -> (input -> IO output)       -- ^ Graph executor
  -> IO ()
runGraphCLIWith desc inputParser executor = do
  let combinedParser = (,) <$> inputParser <*> outputFormatParser
      parserInfo' = info (combinedParser <**> helper)
        ( fullDesc
        <> progDesc (T.unpack desc)
        )
  (input, fmt) <- execParser parserInfo'
  result <- executor input
  TIO.putStrLn (formatOutput fmt result)

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert camelCase to kebab-case.
--
-- @
-- toKebabCase "inputFile" = "input-file"
-- toKebabCase "processURL" = "process-url"
-- @
toKebabCase :: String -> String
toKebabCase = \case
  [] -> []
  (c:cs) -> toLower c : go c cs
  where
    go :: Char -> String -> String
    go _ [] = []
    go prev (c:cs)
      | isUpper c && not (isUpper prev)
          = '-' : toLower c : go c cs
      | otherwise
          = toLower c : go c cs

-- | Generate error message for missing field documentation.
cliFieldError :: Name -> Name -> TH.Type -> String
cliFieldError typeName fieldName fieldType = unlines
  [ "deriveCLIParser: Missing Haddock documentation for field '"
    ++ nameBase fieldName ++ "' in '" ++ nameBase typeName ++ "'"
  , ""
  , "CLI help text comes from Haddock comments. Add documentation:"
  , ""
  , "  " ++ nameBase fieldName ++ " :: " ++ prettyType fieldType
  , "    -- ^ <description of this field>"
  , ""
  , "Also ensure the module has {-# LANGUAGE FieldSelectors #-}"
  ]

-- | Generate error message for nested record type.
nestedRecordError :: Name -> String -> Name -> String
nestedRecordError typeName flag nestedType = unlines
  [ "deriveCLIParser: Nested record type not supported"
  , ""
  , "Field --" ++ flag ++ " in '" ++ nameBase typeName
    ++ "' has type '" ++ nameBase nestedType ++ "' which is a record."
  , ""
  , "CLI derivation only supports flat types. Options:"
  , ""
  , "  1. Flatten the type: move nested fields to top level"
  , "  2. Use a simpler type for the nested field"
  ]

-- | Generate error message for unsupported type.
unsupportedTypeError :: String -> Name -> String
unsupportedTypeError flag typeName = unlines
  [ "deriveCLIParser: Unsupported type for field --" ++ flag
  , ""
  , "Type '" ++ nameBase typeName ++ "' is not supported."
  , ""
  , "Supported types: Text, String, FilePath, Int, Integer, Double, Bool"
  , "                 Maybe a, [a] (where a is a supported type)"
  ]

-- | Pretty-print a type for error messages.
prettyType :: TH.Type -> String
prettyType (ConT n) = nameBase n
prettyType (AppT (ConT n) inner)
  | nameBase n == "Maybe" = "Maybe " ++ prettyType inner
  | nameBase n == "[]"    = "[" ++ prettyType inner ++ "]"
  | otherwise             = nameBase n ++ " " ++ prettyType inner
prettyType (AppT ListT inner) = "[" ++ prettyType inner ++ "]"
prettyType (AppT l r) = prettyType l ++ " " ++ prettyType r
prettyType t = pprint t
