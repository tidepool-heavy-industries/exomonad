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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Deterministic LSP orchestration tools (Tier 1) as Graph DSL nodes.
module ExoMonad.Control.LSPTools
  ( -- * Find Callers
    FindCallersGraph(..)
  , findCallersLogic
  , FindCallersArgs(..)
  , FindCallersResult(..)
  , CallSite(..)

    -- * Find Callees
  , FindCalleesGraph(..)
  , findCalleesLogic
  , FindCalleesArgs(..)
  , FindCalleesResult(..)
  , CalleeInfo(..)

    -- * Show Type
  , ShowTypeGraph(..)
  , showTypeLogic
  , ShowTypeArgs(..)
  , ShowTypeResult(..)
  , TypeField(..)
  , TypeConstructor(..)

    -- * Show Fields
  , ShowFieldsGraph(..)
  , showFieldsLogic
  , ShowFieldsArgs(..)
  , ShowFieldsResult(..)
  , RecordField(..)

    -- * Show Constructors
  , ShowConstructorsGraph(..)
  , showConstructorsLogic
  , ShowConstructorsArgs(..)
  , ShowConstructorsResult(..)
  , Constructor(..)
  , parseADTConstructors
  , parseGADTConstructors
  , stripDerivingClause
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, LastMember, sendM)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import ExoMonad.Effect.LSP
import ExoMonad.Effect.Types (Log, logDebug, Return, returnValue)
import ExoMonad.Role (Role(..))
import ExoMonad.Graph.Generic (type (:-))
import ExoMonad.Graph.Generic.Core (LogicNode)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, GraphEntries, GraphEntry(..))
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))

import ExoMonad.Control.LSPTools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- DERIVATIONS
-- ════════════════════════════════════════════════════════════════════════════

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "fca" } ''FindCallersArgs
  [ 'fcaName         ?? "Function name to find callers of"
  , 'fcaContextLines ?? "Lines of context around each call site"
  , 'fcaMaxResults   ?? "Maximum number of results to return"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "fce" } ''FindCalleesArgs
  [ 'fceName       ?? "Function name to find callees of"
  , 'fceMaxResults ?? "Maximum number of results to return"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "sta" } ''ShowTypeArgs
  [ 'staTypeName ?? "Type name to inspect (record, sum type, or GADT)"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "sfa" } ''ShowFieldsArgs
  [ 'sfaTypeName ?? "Record type name to inspect"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "sca" } ''ShowConstructorsArgs
  [ 'scaTypeName ?? "Sum type name to inspect"
  ])


-- ════════════════════════════════════════════════════════════════════════════
-- FIND-CALLERS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for find_callers tool.
newtype FindCallersGraph mode = FindCallersGraph
  { fcRun :: mode :- LogicNode
      :@ Input FindCallersArgs
      :@ UsesEffects '[Return FindCallersResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for find_callers.
type instance GraphEntries FindCallersGraph =
  '[ "find_callers" ':~> '("fcRun", FindCallersArgs, "Find actual call sites of a function, filtering out imports and type signatures", '[ 'Dev]) ]

-- | Core logic for finding callers.
findCallersLogic
  :: (Member LSP es, Member Log es, Member (Return FindCallersResult) es, LastMember IO es)
  => FindCallersArgs
  -> Eff es FindCallersResult
findCallersLogic args = do
  let name = fcaName args
      ctxLines = fromMaybe 1 (fcaContextLines args)
      maxResults = fromMaybe 50 (fcaMaxResults args)

  -- Check indexing state for warning
  indexingState <- getIndexingState
  let warning = case indexingState of
        Startup -> Just "HLS is starting up..."
        Indexing -> Just "HLS is still indexing. Results may be incomplete."
        Ready -> Nothing

  logDebug $ "[find_callers] Finding callers of: " <> name

  -- Find the symbol's definition
  symbols <- workspaceSymbol name
  let exactMatches = filter (\s -> s.siName == name) symbols

  -- Log workspaceSymbol results
  logDebug $ "[find_callers] workspaceSymbol: query=" <> name
          <> " total=" <> T.pack (show (length symbols))
          <> " exact_matches=" <> T.pack (show (length exactMatches))

  result <- case exactMatches of
    [] -> pure FindCallersResult
      { fcrName = name
      , fcrDefinitionFile = Nothing
      , fcrDefinitionLine = Nothing
      , fcrCallSites = []
      , fcrFilteredCount = 0
      , fcrTruncated = False
      , fcrWarning = warning
      }

    (sym:_) -> do
      let defLoc = sym.siLocation
          defFile = stripFilePrefix defLoc.locUri
          defLine = defLoc.locRange.rangeStart.posLine + 1
          defPos = defLoc.locRange.rangeStart

      -- Get all references
      refs <- references (textDocument defFile) defPos

      -- Log findReferences results
      logDebug $ "[find_callers] references: def=" <> defFile <> ":" <> T.pack (show defLine)
              <> " total_refs=" <> T.pack (show (length refs))

      -- Filter each reference with logging
      callSites <- fmap concat $ forM refs $ \ref -> do
        let file = stripFilePrefix ref.locUri
            line = ref.locRange.rangeStart.posLine + 1
            col = ref.locRange.rangeStart.posCharacter + 1

        -- Skip the definition itself
        if file == defFile && line == defLine
          then do
            logDebug $ "[find_callers] ref: " <> file <> ":" <> T.pack (show line)
                    <> " reason=" <> T.pack (show FRDefinition)
            pure []
          else do
            -- Read context
            ctx <- sendM $ readContextLines (T.unpack file) (line - 1) ctxLines
            let matchLine = ctxMatch ctx
                reason = classifyLine name matchLine

            -- Log per-reference filtering decision
            logDebug $ "[find_callers] ref: " <> file <> ":" <> T.pack (show line)
                    <> " reason=" <> T.pack (show reason)
                    <> " line=" <> T.take 80 matchLine

            -- Apply heuristic filters
            if reason == FRCallSite
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

      -- Log filtering summary
      logDebug $ "[find_callers] summary: total=" <> T.pack (show (length refs))
              <> " call_sites=" <> T.pack (show (length callSites))
              <> " filtered=" <> T.pack (show filteredCount)

      pure FindCallersResult
        { fcrName = name
        , fcrDefinitionFile = Just defFile
        , fcrDefinitionLine = Just defLine
        , fcrCallSites = selected
        , fcrFilteredCount = filteredCount
        , fcrTruncated = truncated
        , fcrWarning = warning
        }

  returnValue result


-- ════════════════════════════════════════════════════════════════════════════
-- FIND-CALLEES GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for find_callees tool.
newtype FindCalleesGraph mode = FindCalleesGraph
  { fceRun :: mode :- LogicNode
      :@ Input FindCalleesArgs
      :@ UsesEffects '[Return FindCalleesResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for find_callees.
type instance GraphEntries FindCalleesGraph =
  '[ "find_callees" ':~> '("fceRun", FindCalleesArgs, "Find functions called by a given function", '[ 'Dev]) ]

-- | Core logic for finding callees.
findCalleesLogic
  :: (Member LSP es, Member Log es, Member (Return FindCalleesResult) es)
  => FindCalleesArgs -> Eff es FindCalleesResult
findCalleesLogic args = do
  let name = fceName args
      maxResults = fromMaybe 50 (fceMaxResults args)

  -- Check indexing state for warning
  indexingState <- getIndexingState
  let warning = case indexingState of
        Startup -> Just "HLS is starting up..."
        Indexing -> Just "HLS is still indexing. Results may be incomplete."
        Ready -> Nothing

  logDebug $ "[find_callees] Finding callees of: " <> name

  -- Find the symbol's definition
  symbols <- workspaceSymbol name
  let exactMatches = filter (\s -> s.siName == name) symbols

  -- Log workspaceSymbol results
  logDebug $ "[find_callees] workspaceSymbol: query=" <> name
          <> " total=" <> T.pack (show (length symbols))
          <> " exact_matches=" <> T.pack (show (length exactMatches))

  case exactMatches of
    [] -> do
      returnValue FindCalleesResult
        { fceResultName = name
        , fceDefinitionFile = Nothing
        , fceDefinitionLine = Nothing
        , fceCallees = []
        , fceTruncated = False
        , fceWarning = warning
        }

    (sym:_) -> do
      let defLoc = sym.siLocation
          defFile = stripFilePrefix defLoc.locUri
          defLine = defLoc.locRange.rangeStart.posLine + 1
          defPos = defLoc.locRange.rangeStart
          docId = textDocument defFile

      -- Prepare call hierarchy
      hierarchyItems <- prepareCallHierarchy docId defPos

      case hierarchyItems of
        [] -> do
          logDebug $ "[find_callees] No call hierarchy items found"
          returnValue FindCalleesResult
            { fceResultName = name
            , fceDefinitionFile = Just defFile
            , fceDefinitionLine = Just defLine
            , fceCallees = []
            , fceTruncated = False
            , fceWarning = warning
            }

        (rootItem:_) -> do
          -- Get outgoing calls
          calls <- outgoingCalls rootItem
          
          logDebug $ "[find_callees] Found " <> T.pack (show (length calls)) <> " raw outgoing calls"

          let allCallees = mapMaybe toCalleeInfo calls
              truncated = length allCallees > maxResults
              selected = take maxResults allCallees

          returnValue FindCalleesResult
            { fceResultName = name
            , fceDefinitionFile = Just defFile
            , fceDefinitionLine = Just defLine
            , fceCallees = selected
            , fceTruncated = truncated
            , fceWarning = warning
            }

  where
    toCalleeInfo :: CallHierarchyOutgoingCall -> Maybe CalleeInfo
    toCalleeInfo call = 
      let item = call.chocTo
          ranges = call.chocFromRanges
          file = stripFilePrefix item.chiUri
          line = item.chiRange.rangeStart.posLine + 1
      in Just CalleeInfo
        { ciName = item.chiName
        , ciFile = file
        , ciLine = line
        , ciCallSites = map (.rangeStart) ranges
        }


-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-TYPE GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for show_type tool.
newtype ShowTypeGraph mode = ShowTypeGraph
  { stRun :: mode :- LogicNode
      :@ Input ShowTypeArgs
      :@ UsesEffects '[Return ShowTypeResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for show_type.
type instance GraphEntries ShowTypeGraph =
  '[ "show_type" ':~> '("stRun", ShowTypeArgs, "Inspect a Haskell type: shows fields (for records) and constructors (for sum types/GADTs)", '[ 'Dev]) ]

-- | Core logic for showing type information.
showTypeLogic
  :: (Member LSP es, Member Log es, Member (Return ShowTypeResult) es, LastMember IO es)
  => ShowTypeArgs
  -> Eff es ShowTypeResult
showTypeLogic args = do
  let typeName = staTypeName args

  -- Check indexing state for warning
  indexingState <- getIndexingState
  let warning = case indexingState of
        Startup -> Just "HLS is starting up..."
        Indexing -> Just "HLS is still indexing. Results may be incomplete."
        Ready -> Nothing

  logDebug $ "[show_type] Looking up: " <> typeName

  -- Find the type
  symbols <- workspaceSymbol typeName
  let typeSymbols = filter isTypeSymbol $
        filter (\s -> s.siName == typeName) symbols

  logDebug $ "[show_type] workspaceSymbol: query=" <> typeName
          <> " total=" <> T.pack (show (length symbols))
          <> " type_symbols=" <> T.pack (show (length typeSymbols))

  result <- case typeSymbols of
    [] -> pure ShowTypeResult
      { strTypeName = typeName
      , strFile = Nothing
      , strLine = Nothing
      , strTypeKind = "unknown"
      , strFields = []
      , strConstructors = []
      , strRawDefinition = ""
      , strWarning = warning
      }

    (sym:_) -> do
      let loc = sym.siLocation
          file = stripFilePrefix loc.locUri
          line = loc.locRange.rangeStart.posLine + 1

      -- Read the type definition
      rawDef <- sendM $ readTypeDefinition (T.unpack file) (line - 1)

      -- Determine type kind and parse accordingly
      let isNewtype = "newtype " `T.isInfixOf` rawDef
          isGADT = "where" `T.isInfixOf` rawDef
          -- Parse record fields
          fields = map toTypeField $ parseRecordFields rawDef
          -- Parse constructors
          constructors = map toTypeConstructor $
            if isGADT
              then parseGADTConstructors rawDef
              else parseADTConstructors rawDef
          -- Determine type kind
          typeKind
            | isNewtype = "newtype"
            | isGADT = "gadt"
            | not (null fields) = "record"
            | otherwise = "sum"

      logDebug $ "[show_type] result: file=" <> file
              <> " line=" <> T.pack (show line)
              <> " type_kind=" <> typeKind
              <> " fields=" <> T.pack (show (length fields))
              <> " constructors=" <> T.pack (show (length constructors))

      pure ShowTypeResult
        { strTypeName = typeName
        , strFile = Just file
        , strLine = Just line
        , strTypeKind = typeKind
        , strFields = fields
        , strConstructors = constructors
        , strRawDefinition = rawDef
        , strWarning = warning
        }

  returnValue result
  where
    toTypeField :: RecordField -> TypeField
    toTypeField rf = TypeField
      { tfName = rfName rf
      , tfType = rfType rf
      , tfStrict = rfStrict rf
      }

    toTypeConstructor :: Constructor -> TypeConstructor
    toTypeConstructor con = TypeConstructor
      { tcName = conName con
      , tcFields = conFields con
      , tcIsRecord = conIsRecord con
      }


-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-FIELDS GRAPH (Legacy)
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for show_fields tool.
newtype ShowFieldsGraph mode = ShowFieldsGraph
  { sfRun :: mode :- LogicNode
      :@ Input ShowFieldsArgs
      :@ UsesEffects '[Return ShowFieldsResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for show_fields.
type instance GraphEntries ShowFieldsGraph =
  '[ "show_fields" ':~> '("sfRun", ShowFieldsArgs, "Show fields of a Haskell record type with their types", '[ 'Dev]) ]

-- | Core logic for showing fields.
showFieldsLogic
  :: (Member LSP es, Member Log es, Member (Return ShowFieldsResult) es, LastMember IO es)
  => ShowFieldsArgs
  -> Eff es ShowFieldsResult
showFieldsLogic args = do
  let typeName = sfaTypeName args

  -- Check indexing state for warning
  indexingState <- getIndexingState
  let warning = case indexingState of
        Startup -> Just "HLS is starting up..."
        Indexing -> Just "HLS is still indexing. Results may be incomplete."
        Ready -> Nothing

  logDebug $ "[show_fields] Looking up: " <> typeName

  -- Find the type
  symbols <- workspaceSymbol typeName
  let typeSymbols = filter isTypeSymbol $
        filter (\s -> s.siName == typeName) symbols

  -- Log workspaceSymbol results
  logDebug $ "[show_fields] workspaceSymbol: query=" <> typeName
          <> " total=" <> T.pack (show (length symbols))
          <> " type_symbols=" <> T.pack (show (length typeSymbols))

  result <- case typeSymbols of
    [] -> pure ShowFieldsResult
      { sfrTypeName = typeName
      , sfrFile = Nothing
      , sfrLine = Nothing
      , sfrIsRecord = False
      , sfrFields = []
      , sfrRawDefinition = ""
      , sfrWarning = warning
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

      -- Log result after parsing
      logDebug $ "[show_fields] result: file=" <> file
              <> " line=" <> T.pack (show line)
              <> " is_record=" <> T.pack (show isRecord)
              <> " fields=" <> T.pack (show (length fields))

      pure ShowFieldsResult
        { sfrTypeName = typeName
        , sfrFile = Just file
        , sfrLine = Just line
        , sfrIsRecord = isRecord
        , sfrFields = fields
        , sfrRawDefinition = rawDef
        , sfrWarning = warning
        }

  returnValue result


-- ════════════════════════════════════════════════════════════════════════════
-- SHOW-CONSTRUCTORS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for show_constructors tool.
newtype ShowConstructorsGraph mode = ShowConstructorsGraph
  { scRun :: mode :- LogicNode
      :@ Input ShowConstructorsArgs
      :@ UsesEffects '[Return ShowConstructorsResult]
  }
  deriving Generic

-- | MCP tool entry point declaration for show_constructors.
type instance GraphEntries ShowConstructorsGraph =
  '[ "show_constructors" ':~> '("scRun", ShowConstructorsArgs, "Show constructors of a Haskell sum type or GADT", '[ 'Dev]) ]

-- | Core logic for showing constructors.
showConstructorsLogic
  :: (Member LSP es, Member Log es, Member (Return ShowConstructorsResult) es, LastMember IO es)
  => ShowConstructorsArgs
  -> Eff es ShowConstructorsResult
showConstructorsLogic args = do
  let typeName = scaTypeName args

  -- Check indexing state for warning
  indexingState <- getIndexingState
  let warning = case indexingState of
        Startup -> Just "HLS is starting up..."
        Indexing -> Just "HLS is still indexing. Results may be incomplete."
        Ready -> Nothing

  logDebug $ "[show_constructors] Looking up: " <> typeName

  -- Find the type
  symbols <- workspaceSymbol typeName
  let typeSymbols = filter isTypeSymbol $
        filter (\s -> s.siName == typeName) symbols

  -- Log workspaceSymbol results
  logDebug $ "[show_constructors] workspaceSymbol: query=" <> typeName
          <> " total=" <> T.pack (show (length symbols))
          <> " type_symbols=" <> T.pack (show (length typeSymbols))

  result <- case typeSymbols of
    [] -> pure ShowConstructorsResult
      { scrTypeName = typeName
      , scrFile = Nothing
      , scrLine = Nothing
      , scrConstructors = []
      , scrIsGADT = False
      , scrRawDefinition = ""
      , scrWarning = warning
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

      -- Log result after parsing
      logDebug $ "[show_constructors] result: file=" <> file
              <> " line=" <> T.pack (show line)
              <> " is_gadt=" <> T.pack (show isGADT)
              <> " constructors=" <> T.pack (show (length constructors))

      pure ShowConstructorsResult
        { scrTypeName = typeName
        , scrFile = Just file
        , scrLine = Just line
        , scrConstructors = constructors
        , scrIsGADT = isGADT
        , scrRawDefinition = rawDef
        , scrWarning = warning
        }

  returnValue result


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Why a reference was filtered (or kept as a call site)
data FilterReason
  = FRDefinition      -- ^ The definition itself
  | FRImport          -- ^ Import statement
  | FRComment         -- ^ Comment line
  | FRTypeSignature   -- ^ Type signature
  | FRModuleHeader    -- ^ Module header
  | FRExportList      -- ^ Export list
  | FRCallSite        -- ^ Actual call site (kept)
  deriving (Show, Eq)

-- | Strip file:// prefix from URI
stripFilePrefix :: Text -> Text
stripFilePrefix uri
  | "file://" `T.isPrefixOf` uri = T.drop 7 uri
  | otherwise = uri

-- | Check if a symbol is a type
isTypeSymbol :: SymbolInformation -> Bool
isTypeSymbol sym = sym.siKind `elem` 
  [SKClass, SKStruct, SKEnum, SKInterface, SKTypeParameter]

-- | Classify a line - returns filter reason or FRCallSite if it's a real call
classifyLine :: Text -> Text -> FilterReason
classifyLine _name line =
  let stripped = T.stripStart line
  in if isImport stripped then FRImport
     else if isComment stripped then FRComment
     else if isTypeSignature stripped then FRTypeSignature
     else if isModuleHeader stripped then FRModuleHeader
     else if isExportList stripped then FRExportList
     else FRCallSite

isImport :: Text -> Bool
isImport line = "import " `T.isPrefixOf` line

isComment :: Text -> Bool
isComment line =
  "--" `T.isPrefixOf` line ||
  "{- " `T.isPrefixOf` line ||
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
         ">=>" `T.isPrefixOf` stripped            -- constraint continuation

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
      -- Strip deriving clause (handles multiline: "deriving" followed by anything)
      -- Pattern: strip everything from "deriving" keyword onwards
      withoutDeriving = stripDerivingClause afterEq
      -- Split by | for multiple constructors
      conStrs = map T.strip $ T.splitOn "|" withoutDeriving
  in mapMaybe parseADTCon conStrs
  where
    parseADTCon str =
      let words' = T.words str
      in case words' of
        [] -> Nothing
        (name:rest) | isUpperCase name ->
          let isRecord = "{" `T.isInfixOf` str
              fields = if isRecord
                then extractRecordFieldTypes str
                else extractPositionalTypes (T.unwords rest)
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

-- | Check if text starts with an upper case letter.
isUpperCase :: Text -> Bool
isUpperCase t = case T.uncons t of
  Just (c, _) -> c >= 'A' && c <= 'Z'
  Nothing -> False

-- | Strip deriving clause from type definition.
--
-- Handles both single-line and multi-line deriving:
--   data Foo = Bar deriving (Show)
--   data Foo = Bar
--     deriving (Show, Eq)
--     deriving anyclass (ToJSON)
stripDerivingClause :: Text -> Text
stripDerivingClause txt =
  -- First handle multi-line by dropping any lines that start with deriving
  let lines' = T.lines txt
      nonDerivingLines = takeWhile (not . isDerivingLine) lines'
      multiLineStripped = T.unlines nonDerivingLines
      -- Then handle single-line by splitting at the "deriving" keyword
      -- We use breakOn " deriving " to avoid matching things like "FooDeriving"
  in case T.breakOn " deriving " multiLineStripped of
       (before, _) -> before
  where
    isDerivingLine line =
      let stripped = T.stripStart line
      in "deriving " `T.isPrefixOf` stripped ||
         "deriving(" `T.isPrefixOf` stripped ||
         stripped == "deriving"

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
         isUpperCase stripped

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