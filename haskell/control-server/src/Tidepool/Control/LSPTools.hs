{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Deterministic LSP orchestration tools (Tier 1) as Graph DSL nodes.
--
-- Each tool is expressed as a mini-graph with MCPExport annotation,
-- dogfooding the graph DSL for simple query functions.
--
-- = Tier 1 Tools (Pure LSP + Basic Parsing)
--
-- - **find_callers**: Find actual call sites (filter imports/type sigs)
-- - **show_fields**: Quick record field lookup
-- - **show_constructors**: Show sum type constructors
--
-- = Design Notes
--
-- These tools are simple request/response functions, but we use the full
-- graph DSL to:
--
-- 1. Dogfood the DSL and identify ergonomic issues
-- 2. Get automatic MCP tool discovery via MCPExport
-- 3. Establish patterns for more complex Tier 2 tools
--
-- See @pain-points/@ for workflow documentation.
module Tidepool.Control.LSPTools
  ( -- * Find Callers
    FindCallersGraph(..)
  , findCallersHandlers
  , findCallersLogic
  , FindCallersArgs(..)
  , FindCallersResult(..)
  , CallSite(..)

    -- * Show Fields
  , ShowFieldsGraph(..)
  , showFieldsHandlers
  , showFieldsLogic
  , ShowFieldsArgs(..)
  , ShowFieldsResult(..)
  , RecordField(..)

    -- * Show Constructors
  , ShowConstructorsGraph(..)
  , showConstructorsHandlers
  , showConstructorsLogic
  , ShowConstructorsArgs(..)
  , ShowConstructorsResult(..)
  , Constructor(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, LastMember, sendM)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Tidepool.Effect.LSP
import Tidepool.Effect.Types (Log, logDebug)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..))


-- ════════════════════════════════════════════════════════════════════════════
-- FIND-CALLERS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for find_callers tool.
data FindCallersArgs = FindCallersArgs
  { fcaName :: Text              -- ^ Function name to find callers of
  , fcaContextLines :: Maybe Int -- ^ Lines of context (default: 1)
  , fcaMaxResults :: Maybe Int   -- ^ Max results (default: 50)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasJSONSchema)

instance FromJSON FindCallersArgs where
  parseJSON = withObject "FindCallersArgs" $ \v ->
    FindCallersArgs
      <$> v .: "name"
      <*> v .:? "context_lines"
      <*> v .:? "max_results"

instance ToJSON FindCallersArgs where
  toJSON args = object
    [ "name" .= fcaName args
    , "context_lines" .= fcaContextLines args
    , "max_results" .= fcaMaxResults args
    ]

-- | A single call site location.
data CallSite = CallSite
  { csFile :: Text
  , csLine :: Int
  , csColumn :: Int
  , csContext :: Text            -- ^ The line containing the call
  , csContextBefore :: [Text]    -- ^ Lines before
  , csContextAfter :: [Text]     -- ^ Lines after
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CallSite where
  toJSON cs = object
    [ "file" .= csFile cs
    , "line" .= csLine cs
    , "column" .= csColumn cs
    , "context" .= csContext cs
    , "context_before" .= csContextBefore cs
    , "context_after" .= csContextAfter cs
    ]

-- | Result of find_callers tool.
data FindCallersResult = FindCallersResult
  { fcrName :: Text
  , fcrDefinitionFile :: Maybe Text
  , fcrDefinitionLine :: Maybe Int
  , fcrCallSites :: [CallSite]
  , fcrFilteredCount :: Int      -- ^ How many references were filtered out
  , fcrTruncated :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FindCallersResult where
  toJSON fcr = object
    [ "name" .= fcrName fcr
    , "definition_file" .= fcrDefinitionFile fcr
    , "definition_line" .= fcrDefinitionLine fcr
    , "call_sites" .= fcrCallSites fcr
    , "filtered_count" .= fcrFilteredCount fcr
    , "truncated" .= fcrTruncated fcr
    ]

-- | Graph definition for find_callers tool.
--
-- Entry → Logic → Exit pattern for simple query tools.
data FindCallersGraph mode = FindCallersGraph
  { fcEntry :: mode :- EntryNode FindCallersArgs
      :@ MCPExport
      :@ MCPToolDef '("find_callers", "Find actual call sites of a function, filtering out imports and type signatures")

  , fcRun :: mode :- LogicNode
      :@ Input FindCallersArgs
      :@ UsesEffects '[Goto Exit FindCallersResult]

  , fcExit :: mode :- ExitNode FindCallersResult
  }
  deriving Generic

-- | Handlers for find_callers graph.
findCallersHandlers
  :: (Member LSP es, Member Log es, LastMember IO es)
  => FindCallersGraph (AsHandler es)
findCallersHandlers = FindCallersGraph
  { fcEntry = Proxy @FindCallersArgs
  , fcRun = findCallersLogic
  , fcExit = Proxy @FindCallersResult
  }

-- | Core logic for finding callers.
findCallersLogic
  :: (Member LSP es, Member Log es, LastMember IO es)
  => FindCallersArgs
  -> Eff es (GotoChoice '[To Exit FindCallersResult])
findCallersLogic args = do
  let name = fcaName args
      ctxLines = fromMaybe 1 (fcaContextLines args)
      maxResults = fromMaybe 50 (fcaMaxResults args)

  logDebug $ "[find_callers] Finding callers of: " <> name

  -- Find the symbol's definition
  symbols <- workspaceSymbol name
  let exactMatches = filter (\s -> s.siName == name) symbols

  result <- case exactMatches of
    [] -> pure FindCallersResult
      { fcrName = name
      , fcrDefinitionFile = Nothing
      , fcrDefinitionLine = Nothing
      , fcrCallSites = []
      , fcrFilteredCount = 0
      , fcrTruncated = False
      }

    (sym:_) -> do
      let defLoc = sym.siLocation
          defFile = stripFilePrefix defLoc.locUri
          defLine = defLoc.locRange.rangeStart.posLine + 1
          defPos = defLoc.locRange.rangeStart

      -- Get all references
      refs <- references (textDocument defFile) defPos

      -- Filter each reference
      callSites <- fmap concat $ forM refs $ \ref -> do
        let file = stripFilePrefix ref.locUri
            line = ref.locRange.rangeStart.posLine + 1
            col = ref.locRange.rangeStart.posCharacter + 1

        -- Skip the definition itself
        if file == defFile && line == defLine
          then pure []
          else do
            -- Read context
            ctx <- sendM $ readContextLines (T.unpack file) (line - 1) ctxLines
            let matchLine = ctxMatch ctx

            -- Apply heuristic filters
            if isCallSite name matchLine
              then pure [CallSite
                { csFile = file
                , csLine = line
                , csColumn = col
                , csContext = matchLine
                , csContextBefore = ctxBefore ctx
                , csContextAfter = ctxAfter ctx
                }]
              else pure []

      let totalRefs = length refs - 1  -- Exclude definition
          filteredCount = totalRefs - length callSites
          truncated = length callSites > maxResults
          selected = take maxResults callSites

      pure FindCallersResult
        { fcrName = name
        , fcrDefinitionFile = Just defFile
        , fcrDefinitionLine = Just defLine
        , fcrCallSites = selected
        , fcrFilteredCount = filteredCount
        , fcrTruncated = truncated
        }

  pure $ gotoExit result


-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-FIELDS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for show_fields tool.
data ShowFieldsArgs = ShowFieldsArgs
  { sfaTypeName :: Text          -- ^ Record type name
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasJSONSchema)

instance FromJSON ShowFieldsArgs where
  parseJSON = withObject "ShowFieldsArgs" $ \v ->
    ShowFieldsArgs <$> v .: "type_name"

instance ToJSON ShowFieldsArgs where
  toJSON args = object ["type_name" .= sfaTypeName args]

-- | A single record field.
data RecordField = RecordField
  { rfName :: Text
  , rfType :: Text
  , rfStrict :: Bool             -- ^ Has ! strictness annotation
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RecordField where
  toJSON rf = object
    [ "name" .= rfName rf
    , "type" .= rfType rf
    , "strict" .= rfStrict rf
    ]

-- | Result of show_fields tool.
data ShowFieldsResult = ShowFieldsResult
  { sfrTypeName :: Text
  , sfrFile :: Maybe Text
  , sfrLine :: Maybe Int
  , sfrIsRecord :: Bool
  , sfrFields :: [RecordField]
  , sfrRawDefinition :: Text     -- ^ Raw definition text for reference
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShowFieldsResult where
  toJSON sfr = object
    [ "type_name" .= sfrTypeName sfr
    , "file" .= sfrFile sfr
    , "line" .= sfrLine sfr
    , "is_record" .= sfrIsRecord sfr
    , "fields" .= sfrFields sfr
    , "raw_definition" .= sfrRawDefinition sfr
    ]

-- | Graph definition for show_fields tool.
data ShowFieldsGraph mode = ShowFieldsGraph
  { sfEntry :: mode :- EntryNode ShowFieldsArgs
      :@ MCPExport
      :@ MCPToolDef '("show_fields", "Show fields of a Haskell record type with their types")

  , sfRun :: mode :- LogicNode
      :@ Input ShowFieldsArgs
      :@ UsesEffects '[Goto Exit ShowFieldsResult]

  , sfExit :: mode :- ExitNode ShowFieldsResult
  }
  deriving Generic

-- | Handlers for show_fields graph.
showFieldsHandlers
  :: (Member LSP es, Member Log es, LastMember IO es)
  => ShowFieldsGraph (AsHandler es)
showFieldsHandlers = ShowFieldsGraph
  { sfEntry = Proxy @ShowFieldsArgs
  , sfRun = showFieldsLogic
  , sfExit = Proxy @ShowFieldsResult
  }

-- | Core logic for showing fields.
showFieldsLogic
  :: (Member LSP es, Member Log es, LastMember IO es)
  => ShowFieldsArgs
  -> Eff es (GotoChoice '[To Exit ShowFieldsResult])
showFieldsLogic args = do
  let typeName = sfaTypeName args

  logDebug $ "[show_fields] Looking up: " <> typeName

  -- Find the type
  symbols <- workspaceSymbol typeName
  let typeSymbols = filter isTypeSymbol $
        filter (\s -> s.siName == typeName) symbols

  result <- case typeSymbols of
    [] -> pure ShowFieldsResult
      { sfrTypeName = typeName
      , sfrFile = Nothing
      , sfrLine = Nothing
      , sfrIsRecord = False
      , sfrFields = []
      , sfrRawDefinition = ""
      }

    (sym:_) -> do
      let loc = sym.siLocation
          file = stripFilePrefix loc.locUri
          line = loc.locRange.rangeStart.posLine + 1

      -- Read the type definition
      rawDef <- sendM $ readTypeDefinition (T.unpack file) (line - 1)

      -- Parse record fields
      let fields = parseRecordFields rawDef
          isRecord = not (null fields)

      pure ShowFieldsResult
        { sfrTypeName = typeName
        , sfrFile = Just file
        , sfrLine = Just line
        , sfrIsRecord = isRecord
        , sfrFields = fields
        , sfrRawDefinition = rawDef
        }

  pure $ gotoExit result


-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-CONSTRUCTORS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for show_constructors tool.
data ShowConstructorsArgs = ShowConstructorsArgs
  { scaTypeName :: Text          -- ^ Type name
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasJSONSchema)

instance FromJSON ShowConstructorsArgs where
  parseJSON = withObject "ShowConstructorsArgs" $ \v ->
    ShowConstructorsArgs <$> v .: "type_name"

instance ToJSON ShowConstructorsArgs where
  toJSON args = object ["type_name" .= scaTypeName args]

-- | A single constructor.
data Constructor = Constructor
  { conName :: Text
  , conFields :: [Text]          -- ^ Field types (positional or record)
  , conIsRecord :: Bool          -- ^ Uses record syntax
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Constructor where
  toJSON con = object
    [ "name" .= conName con
    , "fields" .= conFields con
    , "is_record" .= conIsRecord con
    ]

-- | Result of show_constructors tool.
data ShowConstructorsResult = ShowConstructorsResult
  { scrTypeName :: Text
  , scrFile :: Maybe Text
  , scrLine :: Maybe Int
  , scrConstructors :: [Constructor]
  , scrIsGADT :: Bool
  , scrRawDefinition :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShowConstructorsResult where
  toJSON scr = object
    [ "type_name" .= scrTypeName scr
    , "file" .= scrFile scr
    , "line" .= scrLine scr
    , "constructors" .= scrConstructors scr
    , "is_gadt" .= scrIsGADT scr
    , "raw_definition" .= scrRawDefinition scr
    ]

-- | Graph definition for show_constructors tool.
data ShowConstructorsGraph mode = ShowConstructorsGraph
  { scEntry :: mode :- EntryNode ShowConstructorsArgs
      :@ MCPExport
      :@ MCPToolDef '("show_constructors", "Show constructors of a Haskell sum type or GADT")

  , scRun :: mode :- LogicNode
      :@ Input ShowConstructorsArgs
      :@ UsesEffects '[Goto Exit ShowConstructorsResult]

  , scExit :: mode :- ExitNode ShowConstructorsResult
  }
  deriving Generic

-- | Handlers for show_constructors graph.
showConstructorsHandlers
  :: (Member LSP es, Member Log es, LastMember IO es)
  => ShowConstructorsGraph (AsHandler es)
showConstructorsHandlers = ShowConstructorsGraph
  { scEntry = Proxy @ShowConstructorsArgs
  , scRun = showConstructorsLogic
  , scExit = Proxy @ShowConstructorsResult
  }

-- | Core logic for showing constructors.
showConstructorsLogic
  :: (Member LSP es, Member Log es, LastMember IO es)
  => ShowConstructorsArgs
  -> Eff es (GotoChoice '[To Exit ShowConstructorsResult])
showConstructorsLogic args = do
  let typeName = scaTypeName args

  logDebug $ "[show_constructors] Looking up: " <> typeName

  -- Find the type
  symbols <- workspaceSymbol typeName
  let typeSymbols = filter isTypeSymbol $
        filter (\s -> s.siName == typeName) symbols

  result <- case typeSymbols of
    [] -> pure ShowConstructorsResult
      { scrTypeName = typeName
      , scrFile = Nothing
      , scrLine = Nothing
      , scrConstructors = []
      , scrIsGADT = False
      , scrRawDefinition = ""
      }

    (sym:_) -> do
      let loc = sym.siLocation
          file = stripFilePrefix loc.locUri
          line = loc.locRange.rangeStart.posLine + 1

      -- Read the type definition
      rawDef <- sendM $ readTypeDefinition (T.unpack file) (line - 1)

      -- Parse constructors
      let isGADT = "where" `T.isInfixOf` rawDef
          constructors = if isGADT
            then parseGADTConstructors rawDef
            else parseADTConstructors rawDef

      pure ShowConstructorsResult
        { scrTypeName = typeName
        , scrFile = Just file
        , scrLine = Just line
        , scrConstructors = constructors
        , scrIsGADT = isGADT
        , scrRawDefinition = rawDef
        }

  pure $ gotoExit result


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Strip file:// prefix from URI
stripFilePrefix :: Text -> Text
stripFilePrefix uri
  | "file://" `T.isPrefixOf` uri = T.drop 7 uri
  | otherwise = uri

-- | Check if a symbol is a type
isTypeSymbol :: SymbolInformation -> Bool
isTypeSymbol sym = sym.siKind `elem`
  [SKClass, SKStruct, SKEnum, SKInterface, SKTypeParameter]

-- | Check if a line is an actual call site (not import/type sig/comment)
isCallSite :: Text -> Text -> Bool
isCallSite _name line =
  let stripped = T.stripStart line
  in not (isImport stripped)
  && not (isComment stripped)
  && not (isTypeSignature stripped)
  && not (isModuleHeader stripped)
  && not (isExportList stripped)

isImport :: Text -> Bool
isImport line = "import " `T.isPrefixOf` line

isComment :: Text -> Bool
isComment line =
  "--" `T.isPrefixOf` line ||
  "{-" `T.isPrefixOf` line ||
  "-- |" `T.isPrefixOf` line

isTypeSignature :: Text -> Bool
isTypeSignature line =
  -- Line contains :: and the symbol appears before it
  case T.breakOn "::" line of
    (before, after) -> not (T.null after) && T.any (/= ' ') before

isModuleHeader :: Text -> Bool
isModuleHeader line = "module " `T.isPrefixOf` line

isExportList :: Text -> Bool
isExportList line =
  -- Lines that look like export lists: ( foo, bar, baz
  "(" `T.isPrefixOf` T.stripStart line &&
  not ("<-" `T.isInfixOf` line) &&  -- Not a monadic bind
  not ("=" `T.isInfixOf` line)      -- Not an assignment

-- | Context lines around a reference
data ContextLines = ContextLines
  { ctxBefore :: [Text]
  , ctxMatch :: Text
  , ctxAfter :: [Text]
  }

-- | Read context lines around a line (0-indexed)
readContextLines :: FilePath -> Int -> Int -> IO ContextLines
readContextLines path lineNum numCtx = do
  contents <- readFileUtf8 path
  let allLines = T.lines contents
      totalLines = length allLines
      startLine = max 0 (lineNum - numCtx)
      endLine = min (totalLines - 1) (lineNum + numCtx)
      beforeLines = take (lineNum - startLine) $ drop startLine allLines
      matchLine = if lineNum < totalLines then allLines !! lineNum else ""
      afterLines = take (endLine - lineNum) $ drop (lineNum + 1) allLines
  pure ContextLines
    { ctxBefore = beforeLines
    , ctxMatch = matchLine
    , ctxAfter = afterLines
    }

-- | Read a type definition (tries to get the complete definition)
readTypeDefinition :: FilePath -> Int -> IO Text
readTypeDefinition path startLine = do
  contents <- readFileUtf8 path
  let allLines = T.lines contents
      -- Take lines starting from definition until we hit something that breaks
      defLines = takeDefinition $ drop startLine allLines
  pure $ T.unlines defLines
  where
    takeDefinition [] = []
    takeDefinition (firstLine:rest) =
      firstLine : takeWhile isContinuation rest

    isContinuation line =
      let stripped = T.stripStart line
      in T.null line ||                          -- blank within def
         "  " `T.isPrefixOf` line ||             -- indented
         "\t" `T.isPrefixOf` line ||             -- tab indented
         "|" `T.isPrefixOf` stripped ||          -- sum type alternative
         "," `T.isPrefixOf` stripped ||          -- record field continuation
         "}" `T.isPrefixOf` stripped ||          -- closing brace
         "deriving" `T.isPrefixOf` stripped ||   -- deriving clause
         "=>" `T.isPrefixOf` stripped            -- constraint continuation

-- | Parse record fields from a type definition
parseRecordFields :: Text -> [RecordField]
parseRecordFields def =
  let lines' = T.lines def
      -- Look for lines with :: inside record braces
      fieldLines = filter hasFieldSyntax lines'
  in mapMaybe parseFieldLine fieldLines
  where
    hasFieldSyntax line =
      "::" `T.isInfixOf` line &&
      (not ("data " `T.isPrefixOf` T.stripStart line)) &&
      (not ("type " `T.isPrefixOf` T.stripStart line)) &&
      (not ("newtype " `T.isPrefixOf` T.stripStart line))

    parseFieldLine line =
      case T.breakOn "::" line of
        (namePart, typePart)
          | not (T.null typePart) ->
              let rawName = T.strip $ T.takeWhileEnd (/= ',') $
                            T.takeWhileEnd (/= '{') $ T.strip namePart
                  isStrict = "!" `T.isPrefixOf` rawName
                  name = T.strip $ if isStrict then T.drop 1 rawName else rawName
                  typ = T.strip $ T.drop 2 typePart  -- drop ::
                  cleanType = T.takeWhile (\c -> c /= ',' && c /= '}' && c /= '-') typ
              in if T.null name || not (isValidIdentifier name)
                 then Nothing
                 else Just RecordField
                   { rfName = name
                   , rfType = T.strip cleanType
                   , rfStrict = isStrict
                   }
          | otherwise -> Nothing

    isValidIdentifier t = case T.uncons t of
      Just (c, _) -> c >= 'a' && c <= 'z'
      Nothing -> False

-- | Parse ADT constructors (non-GADT)
parseADTConstructors :: Text -> [Constructor]
parseADTConstructors def =
  let -- Find content after = sign
      afterEq = case T.breakOn "=" def of
        (_, rest) | not (T.null rest) -> T.drop 1 rest
        _ -> ""
      -- Split by | for multiple constructors
      conStrs = map T.strip $ T.splitOn "|" afterEq
  in mapMaybe parseADTCon conStrs
  where
    parseADTCon str =
      let words' = T.words str
      in case words' of
        [] -> Nothing
        (name:_) | isUpperCase name ->
          let isRecord = "{" `T.isInfixOf` str
              fields = if isRecord
                then extractRecordFieldTypes str
                else extractPositionalTypes (T.unwords $ tail words')
          in Just Constructor
            { conName = name
            , conFields = fields
            , conIsRecord = isRecord
            }
        _ -> Nothing

    extractRecordFieldTypes str =
      -- Extract types from { field1 :: Type1, field2 :: Type2 }
      let inner = T.takeWhile (/= '}') $ T.drop 1 $ T.dropWhile (/= '{') str
          parts = T.splitOn "," inner
      in mapMaybe extractType parts

    extractType part =
      case T.breakOn "::" part of
        (_, typ) | not (T.null typ) -> Just $ T.strip $ T.drop 2 typ
        _ -> Nothing

    extractPositionalTypes str =
      -- Simple: just split by spaces, filter types
      filter isType $ T.words str

    isType t = isUpperCase t || "(" `T.isPrefixOf` t || "[" `T.isPrefixOf` t

    isUpperCase t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

-- | Parse GADT constructors
parseGADTConstructors :: Text -> [Constructor]
parseGADTConstructors def =
  let lines' = T.lines def
      -- GADT constructors are lines with ::
      conLines = filter isGADTConLine lines'
  in mapMaybe parseGADTCon conLines
  where
    isGADTConLine line =
      let stripped = T.stripStart line
      in "::" `T.isInfixOf` stripped &&
         not ("data " `T.isPrefixOf` stripped) &&
         isUpperStart stripped

    isUpperStart t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

    parseGADTCon line =
      case T.breakOn "::" line of
        (namePart, typePart)
          | not (T.null typePart) ->
              let name = T.strip namePart
                  typ = T.strip $ T.drop 2 typePart
              in Just Constructor
                { conName = name
                , conFields = [typ]  -- GADT: full type as single "field"
                , conIsRecord = False
                }
          | otherwise -> Nothing

-- | Read file as UTF-8 text
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = do
  bs <- BS.readFile path
  pure $ TE.decodeUtf8With (\_ _ -> Just '?') bs
