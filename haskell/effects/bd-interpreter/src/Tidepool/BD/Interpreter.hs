-- | BD (Beads) effect interpreter - CLI client.
--
-- Implements BD effect by calling the bd CLI tool.
-- Supports both read and write operations.
--
-- = Usage
--
-- @
-- import Tidepool.BD.Interpreter (runBD, runBDIO, BDConfig(..))
-- import Tidepool.Effects.BD
--
-- main = do
--   let config = BDConfig { bcBeadsDir = Nothing, bcQuiet = True }
--   runM $ runBDIO config $ do
--     -- Read
--     maybeBead <- getBead "gt-hda.2.2"
--     -- Write
--     newId <- createBead $ defaultCreateInput { cbiTitle = "New task" }
--     addLabel newId "auto-created"
--     closeBead newId
-- @
module Tidepool.BD.Interpreter
  ( -- * Interpreter
    runBD
  , runBDIO

    -- * Configuration
  , BDConfig(..)
  , defaultBDConfig

    -- * Low-Level CLI Access (Read)
  , bdShow
  , bdDeps
  , bdBlocking
  , bdLabels
  , bdChildren
  , bdListByStatus
  , bdListByType

    -- * Low-Level CLI Access (Write)
  , bdCreate
  , bdUpdate
  , bdClose
  , bdReopen
  , bdAddLabel
  , bdRemoveLabel
  , bdAddDep
  , bdRemoveDep
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Tidepool.Effects.BD
  ( BD(..)
  , BeadInfo(..)
  , BeadType(..)
  , BeadStatus(..)
  , DependencyInfo(..)
  , DependencyType(..)
  , CreateBeadInput(..)
  , UpdateBeadInput(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for BD interpreter.
data BDConfig = BDConfig
  { bcBeadsDir :: Maybe FilePath
    -- ^ Optional beads directory. If Nothing, bd uses auto-discovery.
  , bcQuiet :: Bool
    -- ^ Suppress stderr warnings from bd CLI.
  }
  deriving (Show, Eq)

-- | Default configuration (auto-discovery, suppress warnings).
defaultBDConfig :: BDConfig
defaultBDConfig = BDConfig
  { bcBeadsDir = Nothing
  , bcQuiet = True
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run BD effects with a pure handler.
--
-- This is the main interpreter that takes handler functions.
runBD
  :: (Text -> Eff effs (Maybe BeadInfo))  -- ^ Handler for GetBead
  -> (Text -> Eff effs [BeadInfo])        -- ^ Handler for GetDeps
  -> (Text -> Eff effs [BeadInfo])        -- ^ Handler for GetBlocking
  -> (Text -> Eff effs [Text])            -- ^ Handler for GetLabels
  -> (BeadStatus -> Eff effs [BeadInfo])  -- ^ Handler for ListByStatus
  -> (BeadType -> Eff effs [BeadInfo])    -- ^ Handler for ListByType
  -> Eff (BD ': effs) a
  -> Eff effs a
runBD hGetBead hGetDeps hGetBlocking hGetLabels hListByStatus hListByType = interpret $ \case
  GetBead beadId     -> hGetBead beadId
  GetDeps beadId     -> hGetDeps beadId
  GetBlocking beadId -> hGetBlocking beadId
  GetLabels beadId   -> hGetLabels beadId
  ListByStatus s     -> hListByStatus s
  ListByType t       -> hListByType t
  -- Note: GetChildren and write operations are handled by runBDIO, not runBD
  GetChildren _      -> error "runBD: GetChildren not supported, use runBDIO"
  CreateBead _       -> error "runBD: CreateBead not supported, use runBDIO"
  UpdateBead _ _     -> error "runBD: UpdateBead not supported, use runBDIO"
  CloseBead _        -> error "runBD: CloseBead not supported, use runBDIO"
  ReopenBead _       -> error "runBD: ReopenBead not supported, use runBDIO"
  AddLabel _ _       -> error "runBD: AddLabel not supported, use runBDIO"
  RemoveLabel _ _    -> error "runBD: RemoveLabel not supported, use runBDIO"
  AddDep _ _ _       -> error "runBD: AddDep not supported, use runBDIO"
  RemoveDep _ _      -> error "runBD: RemoveDep not supported, use runBDIO"


-- | Run BD effects using the bd CLI.
--
-- This interpreter shells out to the bd command for each operation.
runBDIO :: LastMember IO effs => BDConfig -> Eff (BD ': effs) a -> Eff effs a
runBDIO config = interpret $ \case
  -- Read operations
  GetBead beadId -> sendM $ bdShow config beadId

  GetDeps beadId -> sendM $ do
    -- Get the bead first, then extract dependencies
    result <- bdShow config beadId
    case result of
      Nothing -> pure []
      Just bead -> do
        -- Fetch full info for each dependency
        beads <- mapM (bdShow config . depId) bead.biDependencies
        pure $ [b | Just b <- beads]

  GetBlocking beadId -> sendM $ do
    -- Get the bead first, then extract dependents
    result <- bdShow config beadId
    case result of
      Nothing -> pure []
      Just bead -> do
        -- Fetch full info for each dependent
        beads <- mapM (bdShow config . depId) bead.biDependents
        pure $ [b | Just b <- beads]

  GetLabels beadId -> sendM $ bdLabels config beadId

  GetChildren parentId -> sendM $ bdChildren config parentId

  ListByStatus status -> sendM $ bdListByStatus config status
  ListByType btype -> sendM $ bdListByType config btype

  -- Write operations
  CreateBead input -> sendM $ bdCreate config input
  UpdateBead beadId input -> sendM $ bdUpdate config beadId input
  CloseBead beadId -> sendM $ bdClose config beadId
  ReopenBead beadId -> sendM $ bdReopen config beadId
  AddLabel beadId label -> sendM $ bdAddLabel config beadId label
  RemoveLabel beadId label -> sendM $ bdRemoveLabel config beadId label
  AddDep fromId toId depType -> sendM $ bdAddDep config fromId toId depType
  RemoveDep fromId toId -> sendM $ bdRemoveDep config fromId toId


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Show a bead by ID using bd CLI.
--
-- Returns Nothing if bead not found or on parse error.
bdShow :: BDConfig -> Text -> IO (Maybe BeadInfo)
bdShow config beadId = do
  let args = ["show", "--json", T.unpack beadId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left _err -> pure Nothing
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        -- bd show --json returns an array with one element
        Right [bead] -> pure $ Just bead
        Right _      -> pure Nothing  -- Empty or multiple (unexpected)
        Left _       -> pure Nothing  -- Parse error


-- | Get dependencies for a bead.
--
-- Note: This is a convenience function. The effect uses GetDeps which
-- calls bdShow and extracts dependencies.
bdDeps :: BDConfig -> Text -> IO [BeadInfo]
bdDeps config beadId = do
  result <- bdShow config beadId
  case result of
    Nothing -> pure []
    Just bead -> do
      beads <- mapM (bdShow config . depId) bead.biDependencies
      pure $ [b | Just b <- beads]


-- | Get beads that depend on this bead (blocking).
--
-- Note: This is a convenience function.
bdBlocking :: BDConfig -> Text -> IO [BeadInfo]
bdBlocking config beadId = do
  result <- bdShow config beadId
  case result of
    Nothing -> pure []
    Just bead -> do
      beads <- mapM (bdShow config . depId) bead.biDependents
      pure $ [b | Just b <- beads]


-- | Extract ID from DependencyInfo.
--
-- Helper to work with NoFieldSelectors in tidepool-core.
depId :: DependencyInfo -> Text
depId (DependencyInfo { diId = i }) = i


-- | Get labels for a bead.
--
-- Uses @bd label list --json@ command.
bdLabels :: BDConfig -> Text -> IO [Text]
bdLabels config beadId = do
  let args = ["label", "list", "--json", T.unpack beadId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left _err -> pure []
    Right output ->
      -- bd label list --json returns ["label1", "label2", ...]
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right labels -> pure labels
        Left _ -> pure []  -- Parse error, return empty


-- | Get child beads (beads with this as parent).
--
-- Uses @bd list --parent <id> --json@ command.
bdChildren :: BDConfig -> Text -> IO [BeadInfo]
bdChildren config parentId = do
  let args = ["list", "--parent", T.unpack parentId, "--json"]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left _err -> pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right beads -> pure beads
        Left _ -> pure []


-- | List beads by status.
--
-- Uses @bd list --status=STATUS --json@ command.
bdListByStatus :: BDConfig -> BeadStatus -> IO [BeadInfo]
bdListByStatus config status = do
  let statusStr = case status of
        StatusOpen       -> "open"
        StatusInProgress -> "in_progress"
        StatusClosed     -> "closed"
        StatusHooked     -> "hooked"
        StatusBlocked    -> "blocked"
  let args = ["list", "--json", "--status=" ++ statusStr]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left _err -> pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right beads -> pure beads
        Left _ -> pure []


-- | List beads by type.
--
-- Uses @bd list --type=TYPE --json@ command.
bdListByType :: BDConfig -> BeadType -> IO [BeadInfo]
bdListByType config btype = do
  let typeStr = case btype of
        TypeTask         -> "task"
        TypeBug          -> "bug"
        TypeFeature      -> "feature"
        TypeEpic         -> "epic"
        TypeMergeRequest -> "merge-request"
        TypeMessage      -> "message"
        TypeMolecule     -> "molecule"
        TypeAgent        -> "agent"
        TypeOther t      -> T.unpack t
  let args = ["list", "--json", "--type=" ++ typeStr]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left _err -> pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right beads -> pure beads
        Left _ -> pure []


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - WRITE
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new bead.
--
-- Uses @bd create@ command. Returns the generated bead ID.
bdCreate :: BDConfig -> CreateBeadInput -> IO Text
bdCreate config input = do
  let baseArgs = ["create", T.unpack input.cbiTitle, "--silent"]
                ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir
                ++ ["--type", beadTypeToArg input.cbiType]
                ++ ["--priority", show input.cbiPriority]
                ++ maybe [] (\desc -> ["--description", T.unpack desc]) input.cbiDescription
                ++ maybe [] (\p -> ["--parent", T.unpack p]) input.cbiParent
                ++ maybe [] (\a -> ["--assignee", T.unpack a]) input.cbiAssignee
                ++ concatMap (\l -> ["--labels", T.unpack l]) input.cbiLabels
                ++ concatMap depToArg input.cbiDeps

  result <- runBdCommand config baseArgs
  case result of
    Left err -> error $ "bd create failed: " <> T.unpack err
    Right output -> pure $ T.strip output


-- | Update an existing bead.
--
-- Uses @bd update@ command.
bdUpdate :: BDConfig -> Text -> UpdateBeadInput -> IO ()
bdUpdate config beadId input = do
  let args = ["update", T.unpack beadId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir
            ++ maybe [] (\t -> ["--title", T.unpack t]) input.ubiTitle
            ++ maybe [] (\d -> ["--description", T.unpack d]) input.ubiDescription
            ++ maybe [] (\s -> ["--status", beadStatusToArg s]) input.ubiStatus
            ++ maybe [] (\p -> ["--priority", show p]) input.ubiPriority
            ++ maybe [] (\a -> ["--assignee", T.unpack a]) input.ubiAssignee

  -- Only run if there are actual changes
  if null (drop 2 args)  -- Just "update" and beadId
    then pure ()
    else do
      result <- runBdCommand config args
      case result of
        Left err -> error $ "bd update failed: " <> T.unpack err
        Right _ -> pure ()


-- | Close a bead.
--
-- Uses @bd close@ command.
bdClose :: BDConfig -> Text -> IO ()
bdClose config beadId = do
  let args = ["close", T.unpack beadId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd close failed: " <> T.unpack err
    Right _ -> pure ()


-- | Reopen a closed bead.
--
-- Uses @bd reopen@ command.
bdReopen :: BDConfig -> Text -> IO ()
bdReopen config beadId = do
  let args = ["reopen", T.unpack beadId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd reopen failed: " <> T.unpack err
    Right _ -> pure ()


-- | Add a label to a bead.
--
-- Uses @bd label add@ command.
bdAddLabel :: BDConfig -> Text -> Text -> IO ()
bdAddLabel config beadId label = do
  let args = ["label", "add", T.unpack beadId, T.unpack label]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd label add failed: " <> T.unpack err
    Right _ -> pure ()


-- | Remove a label from a bead.
--
-- Uses @bd label remove@ command.
bdRemoveLabel :: BDConfig -> Text -> Text -> IO ()
bdRemoveLabel config beadId label = do
  let args = ["label", "remove", T.unpack beadId, T.unpack label]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd label remove failed: " <> T.unpack err
    Right _ -> pure ()


-- | Add a dependency between beads.
--
-- Uses @bd dep add@ command.
bdAddDep :: BDConfig -> Text -> Text -> DependencyType -> IO ()
bdAddDep config fromId toId depType = do
  let args = ["dep", "add", T.unpack fromId, depTypeToArg depType <> ":" <> T.unpack toId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd dep add failed: " <> T.unpack err
    Right _ -> pure ()


-- | Remove a dependency between beads.
--
-- Uses @bd dep remove@ command.
bdRemoveDep :: BDConfig -> Text -> Text -> IO ()
bdRemoveDep config fromId toId = do
  let args = ["dep", "remove", T.unpack fromId, T.unpack toId]
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- runBdCommand config args
  case result of
    Left err -> error $ "bd dep remove failed: " <> T.unpack err
    Right _ -> pure ()


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a bd command and return stdout or error.
runBdCommand :: BDConfig -> [String] -> IO (Either Text Text)
runBdCommand config args = do
  result <- try $ readProcessWithExitCode "bd" args ""
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "bd command failed: " <> T.pack (show e)

    Right (exitCode, stdout, stderr) ->
      case exitCode of
        ExitSuccess -> pure $ Right $ T.pack stdout
        ExitFailure code ->
          -- bd often returns non-zero with warnings but valid output
          -- Check if we got valid JSON despite exit code
          if not (null stdout) && ("id" `T.isInfixOf` T.pack stdout || "{" `T.isPrefixOf` T.pack stdout || "[" `T.isPrefixOf` T.pack stdout)
            then pure $ Right $ T.pack stdout
            else pure $ Left $ "bd exited with code " <> T.pack (show code)
                             <> if config.bcQuiet then "" else ": " <> T.pack stderr


-- | Convert BeadType to CLI argument.
beadTypeToArg :: BeadType -> String
beadTypeToArg TypeTask         = "task"
beadTypeToArg TypeBug          = "bug"
beadTypeToArg TypeFeature      = "feature"
beadTypeToArg TypeEpic         = "epic"
beadTypeToArg TypeMergeRequest = "merge-request"
beadTypeToArg TypeMessage      = "message"
beadTypeToArg TypeMolecule     = "molecule"
beadTypeToArg TypeAgent        = "agent"
beadTypeToArg (TypeOther t)    = T.unpack t


-- | Convert BeadStatus to CLI argument.
beadStatusToArg :: BeadStatus -> String
beadStatusToArg StatusOpen       = "open"
beadStatusToArg StatusInProgress = "in_progress"
beadStatusToArg StatusClosed     = "closed"
beadStatusToArg StatusHooked     = "hooked"
beadStatusToArg StatusBlocked    = "blocked"


-- | Convert DependencyType to CLI argument prefix.
depTypeToArg :: DependencyType -> String
depTypeToArg DepParentChild = "parent-child"
depTypeToArg DepBlocks      = "blocks"
depTypeToArg DepDependsOn   = "depends-on"


-- | Convert dependency tuple to CLI arguments for --deps.
depToArg :: (Text, DependencyType) -> [String]
depToArg (beadId, depType) = ["--deps", depTypeToArg depType <> ":" <> T.unpack beadId]
