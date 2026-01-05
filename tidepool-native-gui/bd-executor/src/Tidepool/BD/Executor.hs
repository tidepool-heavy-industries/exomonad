-- | BD (Beads) effect executor - CLI client.
--
-- Implements BD effect by calling the bd CLI tool.
-- All queries are read-only and use @bd show --json@.
--
-- = Usage
--
-- @
-- import Tidepool.BD.Executor (runBD, runBDIO, BDConfig(..))
-- import Tidepool.Effects.BD (BD, getBead, getDeps)
--
-- main = do
--   let config = BDConfig { bcBeadsDir = Nothing }  -- use auto-discovery
--   runM $ runBDIO config $ do
--     maybeBead <- getBead "gt-hda.2.2"
--     case maybeBead of
--       Nothing -> pure ()
--       Just bead -> print bead.biTitle
-- @
module Tidepool.BD.Executor
  ( -- * Executor
    runBD
  , runBDIO

    -- * Configuration
  , BDConfig(..)
  , defaultBDConfig

    -- * Low-Level CLI Access
  , bdShow
  , bdDeps
  , bdBlocking
  , bdLabels
  , bdListByStatus
  , bdListByType
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Process
  ( readProcessWithExitCode
  , proc
  , CreateProcess(..)
  , StdStream(..)
  , withCreateProcess
  , waitForProcess
  )
import GHC.IO.Handle (hGetContents)
import System.Exit (ExitCode(..))

import Tidepool.Effects.BD (BD(..), BeadInfo(..), BeadStatus(..), BeadType(..), DependencyInfo(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for BD executor.
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
-- EXECUTOR
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


-- | Run BD effects using the bd CLI.
--
-- This interpreter shells out to the bd command for each operation.
runBDIO :: LastMember IO effs => BDConfig -> Eff (BD ': effs) a -> Eff effs a
runBDIO config = interpret $ \case
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

  ListByStatus status -> sendM $ bdListByStatus config status

  ListByType btype -> sendM $ bdListByType config btype


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
          if not (null stdout) && ("id" `T.isInfixOf` T.pack stdout || "{" `T.isPrefixOf` T.pack stdout)
            then pure $ Right $ T.pack stdout
            else pure $ Left $ "bd exited with code " <> T.pack (show code)
                             <> if config.bcQuiet then "" else ": " <> T.pack stderr
