{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | CLI derivation for exomonad graphs.
--
-- This module enables wrapping any exomonad graph as a CLI tool where:
--
-- * EntryNode type → CLI arguments (derived from Haddock via TH)
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
-- Same as 'ExoMonad.Schema.deriveJSONSchema':
--
-- 1. Input type must be in a separate, already-compiled module (TH staging)
-- 2. That module must have @{-\# LANGUAGE FieldSelectors \#-}@
-- 3. All fields must have @-- ^@ Haddock documentation
module ExoMonad.Graph.CLI
  ( -- * GraphCLI Typeclass
    GraphCLI (..),

    -- * Type Families for EntryNode/Exit Extraction
    GraphEntryType,
    GraphExitType,
    ValidEntry,
    ValidExit,

    -- * TH Parser Derivation
    deriveCLIParser,

    -- * Output Formatting
    OutputFormat (..),
    outputFormatParser,
    formatOutput,

    -- * CLI Runner
    runGraphCLIWith,
    runGraphCLIPure,

    -- * Helpers (exported for testing)
    toKebabCase,
  )
where

import Control.Monad.Freer (run)
import Data.Aeson (ToJSON)
import Data.Aeson.Text qualified as Aeson
import Data.Char (isUpper, toLower)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import ExoMonad.Graph.Generic (AsHandler, FieldsWithNamesOf)
import ExoMonad.Graph.Generic.Core (AsGraph, EntryNode, ExitNode)
import ExoMonad.Graph.Interpret (CallHandler, DispatchGoto, FindEntryHandler, runGraph)
import GHC.Generics (Generic (..), K1 (..), M1 (..), (:*:))
import GHC.Generics qualified as G
import GHC.Records (HasField)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified as TH
import Options.Applicative

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

-- | Extract EntryNode type from a graph record's Generic representation.
--
-- @
-- GraphEntryType MyGraph = Input  -- if entry :: mode :- EntryNode Input
-- @
--
-- Produces a helpful type error if no EntryNode is found.
type GraphEntryType :: (Type -> Type) -> Type
type family GraphEntryType graph where
  GraphEntryType graph = ExtractEntry (Rep (graph AsGraph))

-- | Extract Exit type from a graph record's Generic representation.
--
-- @
-- GraphExitType MyGraph = Output  -- if exit :: mode :- ExitNode Output
-- @
--
-- Produces a helpful type error if no Exit is found.
type GraphExitType :: (Type -> Type) -> Type
type family GraphExitType graph where
  GraphExitType graph = ExtractExit (Rep (graph AsGraph))

-- | Extract EntryNode type from Generic representation.
--
-- Traverses the representation looking for @EntryNode a@ field types.
-- Returns 'EntryNotFound' sentinel if not found.
type ExtractEntry :: (Type -> Type) -> Type
type family ExtractEntry f where
  ExtractEntry (M1 G.D _ f) = ExtractEntry f
  ExtractEntry (M1 G.C _ f) = ExtractEntry f
  ExtractEntry (M1 G.S _ (K1 _ (EntryNode a))) = a
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
  ExtractExit (M1 G.S _ (K1 _ (ExitNode a))) = a
  ExtractExit (M1 G.S _ _) = ExitNotFound
  ExtractExit (l :*: r) = ChooseExit (ExtractExit l) (ExtractExit r)
  ExtractExit _ = ExitNotFound

-- | Sentinel for missing EntryNode - will produce a type error when used.
data EntryNotFound

-- | Sentinel for missing Exit - will produce a type error when used.
data ExitNotFound

-- | Choose between EntryNode candidates (take first non-sentinel).
type ChooseEntry :: Type -> Type -> Type
type family ChooseEntry l r where
  ChooseEntry EntryNotFound r = r
  ChooseEntry l _ = l

-- | Choose between Exit candidates (take first non-sentinel).
type ChooseExit :: Type -> Type -> Type
type family ChooseExit l r where
  ChooseExit ExitNotFound r = r
  ChooseExit l _ = l

-- | Constraint that validates EntryNode type was found.
-- Produces a helpful type error if 'EntryNotFound' sentinel is present.
type ValidEntry :: Type -> Constraint
type family ValidEntry t where
  ValidEntry EntryNotFound =
    TypeError
      ( 'Text "GraphCLI: Could not find EntryNode in graph."
          ':$$: 'Text ""
          ':$$: 'Text "Ensure your graph record has an EntryNode field:"
          ':$$: 'Text "  data MyGraph mode = MyGraph"
          ':$$: 'Text "    { entry :: mode :- EntryNode YourInputType"
          ':$$: 'Text "    , ..."
          ':$$: 'Text "    }"
      )
  ValidEntry _ = ()

-- | Constraint that validates Exit type was found.
-- Produces a helpful type error if 'ExitNotFound' sentinel is present.
type ValidExit :: Type -> Constraint
type family ValidExit t where
  ValidExit ExitNotFound =
    TypeError
      ( 'Text "GraphCLI: Could not find Exit in graph."
          ':$$: 'Text ""
          ':$$: 'Text "Ensure your graph record has an Exit field:"
          ':$$: 'Text "  data MyGraph mode = MyGraph"
          ':$$: 'Text "    { ..."
          ':$$: 'Text "    , exit :: mode :- ExitNode YourOutputType"
          ':$$: 'Text "    }"
      )
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
    TyConI (DataD _ _ _ _ cons _)
      | length cons > 1 ->
          deriveSubcommandParser typeName cons
    _ ->
      fail $
        "deriveCLIParser: "
          ++ show typeName
          ++ " must be a record type or sum type with record constructors"

-- | Derive parser for a single-constructor record.
deriveRecordParser :: Name -> Name -> [VarBangType] -> Q Exp
deriveRecordParser typeName conName fields = do
  fieldParsers <- mapM (deriveFieldParser typeName) fields
  let con = conE conName
  -- Build: ConName <$> p1 <*> p2 <*> ...
  case fieldParsers of
    [] -> [|pure $con|]
    (p : ps) ->
      foldl'
        (\acc fp -> [|$acc <*> $(pure fp)|])
        [|$con <$> $(pure p)|]
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
    helpMod = [|help $(litE (stringL helpText))|]

    go :: TH.Type -> Q Exp
    go = \case
      -- Text → strOption
      ConT n
        | nameBase n == "Text" ->
            [|T.pack <$> strOption (long flag <> metavar "TEXT" <> $helpMod)|]
      -- String → strOption
      ConT n
        | nameBase n == "String" ->
            [|strOption (long flag <> metavar "TEXT" <> $helpMod)|]
      -- FilePath (alias for String) → strOption
      ConT n
        | nameBase n == "FilePath" ->
            [|strOption (long flag <> metavar "PATH" <> $helpMod)|]
      -- Int → option auto
      ConT n
        | nameBase n == "Int" ->
            [|option auto (long flag <> metavar "INT" <> $helpMod)|]
      -- Integer → option auto
      ConT n
        | nameBase n == "Integer" ->
            [|option auto (long flag <> metavar "INT" <> $helpMod)|]
      -- Double → option auto
      ConT n
        | nameBase n == "Double" ->
            [|option auto (long flag <> metavar "NUM" <> $helpMod)|]
      -- Bool → switch
      ConT n
        | nameBase n == "Bool" ->
            [|switch (long flag <> $helpMod)|]
      -- Maybe a → optional
      AppT (ConT n) inner | nameBase n == "Maybe" -> do
        innerParser <- go inner
        [|optional $(pure innerParser)|]

      -- [a] → many
      AppT ListT inner -> do
        innerParser <- go inner
        [|many $(pure innerParser)|]

      -- Nested record → error
      ConT n -> do
        nInfo <- reify n
        case nInfo of
          TyConI (DataD _ _ _ _ [RecC _ _] _) ->
            fail $ nestedRecordError typeName flag n
          TyConI (NewtypeD _ _ _ _ (RecC _ _) _) ->
            fail $ nestedRecordError typeName flag n
          _ -> fail $ T.unpack $ unsupportedTypeError flag n
      other ->
        fail $ T.unpack $
          T.pack $ "deriveCLIParser: Unsupported field type for --"
            ++ flag
            ++ ": "
            ++ pprint other

-- | Derive subcommand parser from sum type constructors.
deriveSubcommandParser :: Name -> [Con] -> Q Exp
deriveSubcommandParser typeName cons = do
  commands <- mapM (deriveCommand typeName) cons
  case commands of
    [] -> fail "deriveCLIParser: Sum type must have at least one constructor"
    [c] -> [|subparser $(pure c)|]
    (c : cs) -> do
      let combined =
            foldl'
              (\acc cmd -> [|$acc <> $(pure cmd)|])
              (pure c)
              cs
      [|subparser $combined|]

-- | Derive a single subcommand from a constructor.
deriveCommand :: Name -> Con -> Q Exp
deriveCommand typeName con = case con of
  RecC conName fields -> do
    parser <- deriveRecordParser typeName conName fields
    let cmdName = toKebabCase (nameBase conName)
    mDoc <- getDoc (DeclDoc conName)
    let desc = fromMaybe ("Run " ++ nameBase conName) mDoc
    [|command cmdName (info $(pure parser) (progDesc $(litE (stringL desc))))|]
  NormalC conName [] -> do
    -- Nullary constructor → command with no arguments
    let cmdName = toKebabCase (nameBase conName)
        con' = conE conName
    mDoc <- getDoc (DeclDoc conName)
    let desc = fromMaybe ("Run " ++ nameBase conName) mDoc
    [|command cmdName (info (pure $con') (progDesc $(litE (stringL desc))))|]
  _ ->
    fail $
      "deriveCLIParser: Constructor "
        ++ show con
        ++ " must be a record or nullary constructor"

-- ════════════════════════════════════════════════════════════════════════════
-- OUTPUT FORMATTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Output format for CLI results.
data OutputFormat
  = -- | Output as JSON
    FormatJSON
  | -- | Output as text (via Show)
    FormatText
  deriving (Eq, Show)

-- | Parser for @--format@ flag.
--
-- @
-- --format json   → FormatJSON
-- --format text   → FormatText (default)
-- @
outputFormatParser :: Parser OutputFormat
outputFormatParser =
  option
    (eitherReader parseFormat)
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
      _ -> Left $ "Unknown format '" ++ s ++ "'. Use 'json' or 'text'."

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
runGraphCLIWith ::
  (Show output, ToJSON output) =>
  -- | Description for --help
  Text ->
  -- | Input parser (use deriveCLIParser)
  Parser input ->
  -- | Graph interpreter
  (input -> IO output) ->
  IO ()
runGraphCLIWith desc inputParser interpreter = do
  let combinedParser = (,) <$> inputParser <*> outputFormatParser
      parserInfo' =
        info
          (combinedParser <**> helper)
          ( fullDesc
              <> progDesc (T.unpack desc)
          )
  (input, fmt) <- execParser parserInfo'
  result <- interpreter input
  TIO.putStrLn (formatOutput fmt result)

-- | Run a logic-only graph (no LLM/IO effects) as CLI.
--
-- This wires a graph with empty effect stack @'[]@ directly to CLI.
-- For graphs with LLM/IO effects, use @runGraphCLI@ from exomonad-platform.
--
-- = Example
--
-- @
-- main :: IO ()
-- main = runGraphCLIPure
--   "Counter graph CLI"
--   $(deriveCLIParser ''CounterInput)
--   counterHandlers
-- @
--
-- = Constraints
--
-- The graph must:
--
-- * Have @EntryNode@ and @Exit@ fields (detected via @GraphEntryType@ and @GraphExitType@)
-- * Have a handler that accepts the entry type via @Needs@ (found via @FindEntryHandler@)
-- * Use empty effect stack @'[]@ (all handlers must be pure)
-- * Exit type must have @Show@ and @ToJSON@ instances for output formatting
runGraphCLIPure ::
  forall graph entryHandlerName handler targets.
  ( Show (GraphExitType graph),
    ToJSON (GraphExitType graph),
    Generic (graph AsGraph),
    FindEntryHandler (GraphEntryType graph) (FieldsWithNamesOf graph) ~ 'Just entryHandlerName,
    KnownSymbol entryHandlerName,
    HasField entryHandlerName (graph (AsHandler '[])) handler,
    CallHandler handler (GraphEntryType graph) '[] targets,
    DispatchGoto graph targets '[] (GraphExitType graph)
  ) =>
  -- | Description for --help
  Text ->
  -- | Input parser (use deriveCLIParser)
  Parser (GraphEntryType graph) ->
  -- | Handlers (empty effect stack)
  graph (AsHandler '[]) ->
  IO ()
runGraphCLIPure desc parser handlers =
  runGraphCLIWith desc parser $ \input ->
    pure $ run $ runGraph @graph @(GraphEntryType graph) @targets @(GraphExitType graph) @'[] @entryHandlerName @handler handlers input

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
  (c : cs) -> toLower c : go c cs
  where
    go :: Char -> String -> String
    go _ [] = []
    go prev (c : cs)
      | isUpper c && not (isUpper prev) =
          '-' : toLower c : go c cs
      | otherwise =
          toLower c : go c cs

-- | Generate error message for missing field documentation.
cliFieldError :: Name -> Name -> TH.Type -> String
cliFieldError typeName fieldName fieldType =
  T.unpack $ T.unlines
    [ T.pack $ "deriveCLIParser: Missing Haddock documentation for field '"
        ++ nameBase fieldName
        ++ "' in '"
        ++ nameBase typeName
        ++ "'",
      "",
      "CLI help text comes from Haddock comments. Add documentation:",
      "",
      T.pack $ "  " ++ nameBase fieldName ++ " :: " ++ prettyType fieldType,
      "    -- ^ <description of this field>",
      "",
      "Also ensure the module has {-# LANGUAGE FieldSelectors #-}"
    ]

-- | Generate error message for nested record type.
nestedRecordError :: Name -> String -> Name -> String
nestedRecordError typeName flagName nestedType =
  T.unpack $ T.unlines
    [ T.pack $ "deriveCLIParser: Nested record type not supported",
      "",
      T.pack $ "Field --"
        ++ flagName
        ++ " in '"
        ++ nameBase typeName
        ++ "' has type '"
        ++ nameBase nestedType
        ++ "' which is a record.",
      "",
      "CLI derivation only supports flat types. Options:",
      "",
      "  1. Flatten the type: move nested fields to top level",
      "  2. Use a simpler type for the nested field"
    ]

-- | Generate error message for unsupported type.
unsupportedTypeError :: String -> Name -> Text
unsupportedTypeError flagName typeName =
  T.unlines
    [ T.pack $ "deriveCLIParser: Unsupported type for field --" ++ flagName,
      "",
      T.pack $ "Type '" ++ nameBase typeName ++ "' is not supported.",
      "",
      "Supported types: Text, String, FilePath, Int, Integer, Double, Bool",
      "                 Maybe a, [a] (where a is a supported type)"
    ]

-- | Pretty-print a type for error messages.
prettyType :: TH.Type -> String
prettyType (ConT n) = nameBase n
prettyType (AppT (ConT n) inner)
  | nameBase n == "Maybe" = "Maybe " ++ prettyType inner
  | nameBase n == "[]" = "[" ++ prettyType inner ++ "]"
  | otherwise = nameBase n ++ " " ++ prettyType inner
prettyType (AppT ListT inner) = "[" ++ prettyType inner ++ "]"
prettyType (AppT l r) = prettyType l ++ " " ++ prettyType r
prettyType t = pprint t
