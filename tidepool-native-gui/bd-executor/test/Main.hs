-- | BD executor integration tests.
--
-- Tests JSON parsing and CLI integration with mock bd commands.
-- Uses System.IO.Temp for isolated test environments.
module Main (main) where

import Control.Exception (try, SomeException)
import Control.Monad (forM_, when, void)
import Control.Monad.Freer (Eff, runM)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import System.Environment (setEnv, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode, callProcess)
import Test.Hspec

import Tidepool.Effects.BD
import Tidepool.BD.Executor


main :: IO ()
main = hspec $ do
  describe "BD Types - JSON Parsing" $ do
    beadStatusTests
    beadTypeTests
    dependencyTypeTests
    dependencyInfoTests
    beadInfoTests

  describe "BD Executor - Mock CLI" $ do
    executorTests

  describe "BD Executor - Real CLI" $ do
    realBdTests


-- ════════════════════════════════════════════════════════════════════════════
-- BEAD STATUS TESTS
-- ════════════════════════════════════════════════════════════════════════════

beadStatusTests :: Spec
beadStatusTests = describe "BeadStatus" $ do
  it "parses 'open'" $ do
    eitherDecode "\"open\"" `shouldBe` Right StatusOpen

  it "parses 'in_progress'" $ do
    eitherDecode "\"in_progress\"" `shouldBe` Right StatusInProgress

  it "parses 'closed'" $ do
    eitherDecode "\"closed\"" `shouldBe` Right StatusClosed

  it "parses 'hooked'" $ do
    eitherDecode "\"hooked\"" `shouldBe` Right StatusHooked

  it "parses 'blocked'" $ do
    eitherDecode "\"blocked\"" `shouldBe` Right StatusBlocked

  it "round-trips all statuses" $ do
    forM_ [minBound..maxBound] $ \status ->
      eitherDecode (encode status) `shouldBe` Right (status :: BeadStatus)


-- ════════════════════════════════════════════════════════════════════════════
-- BEAD TYPE TESTS
-- ════════════════════════════════════════════════════════════════════════════

beadTypeTests :: Spec
beadTypeTests = describe "BeadType" $ do
  it "parses 'task'" $ do
    eitherDecode "\"task\"" `shouldBe` Right TypeTask

  it "parses 'bug'" $ do
    eitherDecode "\"bug\"" `shouldBe` Right TypeBug

  it "parses 'feature'" $ do
    eitherDecode "\"feature\"" `shouldBe` Right TypeFeature

  it "parses 'epic'" $ do
    eitherDecode "\"epic\"" `shouldBe` Right TypeEpic

  it "parses 'merge-request'" $ do
    eitherDecode "\"merge-request\"" `shouldBe` Right TypeMergeRequest

  it "parses 'message'" $ do
    eitherDecode "\"message\"" `shouldBe` Right TypeMessage

  it "parses 'molecule'" $ do
    eitherDecode "\"molecule\"" `shouldBe` Right TypeMolecule

  it "parses 'agent'" $ do
    eitherDecode "\"agent\"" `shouldBe` Right TypeAgent

  it "parses unknown types as TypeOther" $ do
    eitherDecode "\"custom-type\"" `shouldBe` Right (TypeOther "custom-type")

  it "round-trips known types" $ do
    forM_ [TypeTask, TypeBug, TypeFeature, TypeEpic, TypeMergeRequest, TypeMessage, TypeMolecule, TypeAgent] $ \t ->
      eitherDecode (encode t) `shouldBe` Right t


-- ════════════════════════════════════════════════════════════════════════════
-- DEPENDENCY TYPE TESTS
-- ════════════════════════════════════════════════════════════════════════════

dependencyTypeTests :: Spec
dependencyTypeTests = describe "DependencyType" $ do
  it "parses 'parent-child'" $ do
    eitherDecode "\"parent-child\"" `shouldBe` Right DepParentChild

  it "parses 'blocks'" $ do
    eitherDecode "\"blocks\"" `shouldBe` Right DepBlocks

  it "parses 'depends-on'" $ do
    eitherDecode "\"depends-on\"" `shouldBe` Right DepDependsOn


-- ════════════════════════════════════════════════════════════════════════════
-- DEPENDENCY INFO TESTS
-- ════════════════════════════════════════════════════════════════════════════

dependencyInfoTests :: Spec
dependencyInfoTests = describe "DependencyInfo" $ do
  it "parses a complete dependency" $ do
    let json = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines
          [ "{"
          , "  \"id\": \"gt-hda.1\","
          , "  \"title\": \"Foundation Layer\","
          , "  \"status\": \"open\","
          , "  \"priority\": 1,"
          , "  \"issue_type\": \"epic\","
          , "  \"dependency_type\": \"parent-child\""
          , "}"
          ]
    let expected = DependencyInfo
          { diId = "gt-hda.1"
          , diTitle = "Foundation Layer"
          , diStatus = StatusOpen
          , diPriority = 1
          , diType = TypeEpic
          , diDepType = DepParentChild
          }
    eitherDecode json `shouldBe` Right expected


-- ════════════════════════════════════════════════════════════════════════════
-- BEAD INFO TESTS
-- ════════════════════════════════════════════════════════════════════════════

beadInfoTests :: Spec
beadInfoTests = describe "BeadInfo" $ do
  it "parses minimal bead info" $ do
    let json = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines
          [ "{"
          , "  \"id\": \"test-1\","
          , "  \"title\": \"Test Issue\","
          , "  \"status\": \"open\","
          , "  \"priority\": 2,"
          , "  \"issue_type\": \"task\""
          , "}"
          ]
    case eitherDecode json of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right (bead :: BeadInfo) -> do
        bead.biId `shouldBe` "test-1"
        bead.biTitle `shouldBe` "Test Issue"
        bead.biStatus `shouldBe` StatusOpen
        bead.biPriority `shouldBe` 2
        bead.biType `shouldBe` TypeTask
        bead.biDescription `shouldBe` Nothing
        bead.biAssignee `shouldBe` Nothing

  it "parses full bead info from bd show --json format" $ do
    let json = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines
          [ "[{"
          , "  \"id\": \"gt-hda.1\","
          , "  \"title\": \"Foundation - LSP/BD Integration Layer\","
          , "  \"description\": \"Build the integration layer\","
          , "  \"status\": \"open\","
          , "  \"priority\": 1,"
          , "  \"issue_type\": \"epic\","
          , "  \"assignee\": \"tidepool/polecats/slit\","
          , "  \"created_at\": \"2026-01-03T21:20:49Z\","
          , "  \"created_by\": \"mayor\","
          , "  \"updated_at\": \"2026-01-03T21:38:43Z\","
          , "  \"parent\": \"gt-hda\","
          , "  \"dependencies\": [{"
          , "    \"id\": \"gt-hda\","
          , "    \"title\": \"Urchin Context Infrastructure\","
          , "    \"status\": \"hooked\","
          , "    \"priority\": 2,"
          , "    \"issue_type\": \"epic\","
          , "    \"dependency_type\": \"parent-child\""
          , "  }],"
          , "  \"dependents\": [{"
          , "    \"id\": \"gt-hda.2\","
          , "    \"title\": \"Target 1 - Context Construction\","
          , "    \"status\": \"open\","
          , "    \"priority\": 2,"
          , "    \"issue_type\": \"epic\","
          , "    \"dependency_type\": \"blocks\""
          , "  }]"
          , "}]"
          ]
    case eitherDecode json of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right [bead :: BeadInfo] -> do
        bead.biId `shouldBe` "gt-hda.1"
        bead.biTitle `shouldBe` "Foundation - LSP/BD Integration Layer"
        bead.biDescription `shouldBe` Just "Build the integration layer"
        bead.biStatus `shouldBe` StatusOpen
        bead.biPriority `shouldBe` 1
        bead.biType `shouldBe` TypeEpic
        bead.biAssignee `shouldBe` Just "tidepool/polecats/slit"
        bead.biParent `shouldBe` Just "gt-hda"
        length bead.biDependencies `shouldBe` 1
        length bead.biDependents `shouldBe` 1
      Right other -> expectationFailure $ "Expected single-element array, got: " ++ show (length other)

  it "handles missing optional fields" $ do
    let json = LBS.fromStrict $ TE.encodeUtf8 $ T.unlines
          [ "{"
          , "  \"id\": \"simple-1\","
          , "  \"title\": \"Simple Task\","
          , "  \"status\": \"in_progress\","
          , "  \"priority\": 0,"
          , "  \"issue_type\": \"bug\""
          , "}"
          ]
    case eitherDecode json of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right (bead :: BeadInfo) -> do
        bead.biDescription `shouldBe` Nothing
        bead.biAssignee `shouldBe` Nothing
        bead.biCreatedAt `shouldBe` Nothing
        bead.biCreatedBy `shouldBe` Nothing
        bead.biUpdatedAt `shouldBe` Nothing
        bead.biParent `shouldBe` Nothing
        bead.biDependencies `shouldBe` []
        bead.biDependents `shouldBe` []


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR TESTS
-- ════════════════════════════════════════════════════════════════════════════

executorTests :: Spec
executorTests = describe "Executor CLI Integration" $ do
  it "bdShow returns Nothing for non-existent bead" $ do
    withMockBdEnv $ \tmpDir -> do
      -- Create a mock bd that returns exit code 1 (not found)
      writeMockBd tmpDir "exit 1"

      let config = BDConfig { bcBeadsDir = Just tmpDir, bcQuiet = True }
      result <- bdShow config "nonexistent"
      result `shouldBe` Nothing

  it "bdShow parses valid JSON from mock bd" $ do
    withMockBdEnv $ \tmpDir -> do
      -- Create a mock bd that returns valid JSON using here-doc
      let mockScript = unlines
            [ "cat <<'MOCKJSON'"
            , "[{"
            , "  \"id\": \"test-123\","
            , "  \"title\": \"Test Bead\","
            , "  \"status\": \"open\","
            , "  \"priority\": 1,"
            , "  \"issue_type\": \"task\""
            , "}]"
            , "MOCKJSON"
            ]
      writeMockBd tmpDir mockScript

      let config = BDConfig { bcBeadsDir = Just tmpDir, bcQuiet = True }
      result <- bdShow config "test-123"
      case result of
        Nothing -> expectationFailure "Expected bead, got Nothing"
        Just bead -> do
          bead.biId `shouldBe` "test-123"
          bead.biTitle `shouldBe` "Test Bead"

  it "bdLabels parses JSON array of labels" $ do
    withMockBdEnv $ \tmpDir -> do
      -- Create a mock bd that returns JSON array (as bd label list --json does)
      writeMockBd tmpDir "echo '[\"infrastructure\",\"urgent\",\"blocked\"]'"

      let config = BDConfig { bcBeadsDir = Just tmpDir, bcQuiet = True }
      result <- bdLabels config "test-123"
      result `shouldBe` ["infrastructure", "urgent", "blocked"]

  it "bdLabels handles empty JSON array" $ do
    withMockBdEnv $ \tmpDir -> do
      -- bd label list --json returns [] for beads with no labels
      writeMockBd tmpDir "echo '[]'"

      let config = BDConfig { bcBeadsDir = Just tmpDir, bcQuiet = True }
      result <- bdLabels config "test-123"
      result `shouldBe` []

  it "runBD pure interpreter works" $ do
    -- Test the pure handler path
    let mockGetBead _ = pure $ Just BeadInfo
          { biId = "mock-1"
          , biTitle = "Mock Bead"
          , biDescription = Nothing
          , biStatus = StatusOpen
          , biPriority = 1
          , biType = TypeTask
          , biAssignee = Nothing
          , biCreatedAt = Nothing
          , biCreatedBy = Nothing
          , biUpdatedAt = Nothing
          , biParent = Nothing
          , biDependencies = []
          , biDependents = []
          }
    let mockGetDeps _ = pure []
    let mockGetBlocking _ = pure []
    let mockGetLabels _ = pure ["test-label"]

    result <- runM $ runBD mockGetBead mockGetDeps mockGetBlocking mockGetLabels $ do
      bead <- getBead "any"
      labels <- getLabels "any"
      pure (bead, labels)

    case result of
      (Just bead, labels) -> do
        bead.biId `shouldBe` "mock-1"
        labels `shouldBe` ["test-label"]
      (Nothing, _) -> expectationFailure "Expected bead"


-- ════════════════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a test with a temporary directory containing a mock bd executable.
--
-- The mock bd script is placed in tmpDir/bin/bd and PATH is modified
-- to find it first.
withMockBdEnv :: (FilePath -> IO a) -> IO a
withMockBdEnv action = withSystemTempDirectory "bd-test" $ \tmpDir -> do
  -- Create bin directory for mock bd
  let binDir = tmpDir </> "bin"
  createDirectoryIfMissing True binDir

  -- Save original PATH and prepend our bin dir
  origPath <- lookupEnv "PATH"
  let newPath = binDir ++ maybe "" (":" ++) origPath
  setEnv "PATH" newPath

  -- Run the test
  result <- action tmpDir

  -- Restore PATH (best effort)
  case origPath of
    Just p -> setEnv "PATH" p
    Nothing -> pure ()

  pure result


-- | Write a mock bd script that executes the given shell command.
--
-- Note: The script echoes to stdout; single quotes in the body need escaping.
writeMockBd :: FilePath -> String -> IO ()
writeMockBd tmpDir body = do
  let bdPath = tmpDir </> "bin" </> "bd"
  TIO.writeFile bdPath $ T.pack $ unlines
    [ "#!/bin/sh"
    , "# Mock bd script for testing"
    , body
    ]
  -- Make executable
  perms <- getPermissions bdPath
  setPermissions bdPath (setOwnerExecutable True perms)


-- ════════════════════════════════════════════════════════════════════════════
-- REAL BD CLI TESTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if the real bd CLI is available and functional.
isBdAvailable :: IO Bool
isBdAvailable = do
  result <- try $ readProcessWithExitCode "bd" ["--help"] ""
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right (ExitSuccess, _, _) -> True
    Right _ -> False


-- | Test fixture data: parent and child bead IDs created by withTestBeadsDb
data TestFixture = TestFixture
  { tfTmpDir   :: FilePath  -- ^ Temp directory containing .beads/
  , tfParentId :: Text      -- ^ ID of parent epic
  , tfChildId  :: Text      -- ^ ID of child task that depends on parent
  }

-- | Create an isolated beads database for testing.
--
-- Creates a temp directory with:
-- - Git repository (bd requires git)
-- - Beads database initialized with prefix "test"
-- - Two test issues: parent (epic) and child (task that depends on parent)
-- - Labels on parent: "infrastructure", "urgent"
withTestBeadsDb :: (TestFixture -> IO a) -> IO a
withTestBeadsDb action = withSystemTempDirectory "bd-real-test" $ \tmpDir -> do
  -- Initialize git repo (bd requires git)
  callProcess "git" ["-C", tmpDir, "init", "--initial-branch=main"]
  callProcess "git" ["-C", tmpDir, "config", "user.email", "test@test.com"]
  callProcess "git" ["-C", tmpDir, "config", "user.name", "Test"]

  -- Create an initial commit (bd may require this)
  TIO.writeFile (tmpDir </> "README.md") "Test repo"
  callProcess "git" ["-C", tmpDir, "add", "."]
  callProcess "git" ["-C", tmpDir, "commit", "-m", "Initial commit"]

  -- Run bd init in the temp directory
  (exitCode, _, stderr) <- readProcessWithExitCode "sh"
    ["-c", "cd " ++ tmpDir ++ " && bd --sandbox --quiet init --prefix test --skip-hooks --skip-merge-driver"]
    ""

  when (exitCode /= ExitSuccess) $
    error $ "bd init failed: " ++ stderr

  -- Create parent epic - capture the generated ID
  (_, parentIdRaw, _) <- readProcessWithExitCode "sh"
    ["-c", "cd " ++ tmpDir ++ " && bd --sandbox create 'Parent Epic' --type epic -p 1 --silent"]
    ""
  let parentId = T.strip $ T.pack parentIdRaw

  -- Create child task that depends on parent
  (_, childIdRaw, _) <- readProcessWithExitCode "sh"
    ["-c", "cd " ++ tmpDir ++ " && bd --sandbox create 'Child Task' --type task -p 2 --deps " ++ T.unpack parentId ++ " --silent"]
    ""
  let childId = T.strip $ T.pack childIdRaw

  -- Add labels to parent
  void $ readProcessWithExitCode "sh"
    ["-c", "cd " ++ tmpDir ++ " && bd --sandbox label add " ++ T.unpack parentId ++ " infrastructure"]
    ""
  void $ readProcessWithExitCode "sh"
    ["-c", "cd " ++ tmpDir ++ " && bd --sandbox label add " ++ T.unpack parentId ++ " urgent"]
    ""

  -- Run the test with fixture data
  action TestFixture
    { tfTmpDir = tmpDir
    , tfParentId = parentId
    , tfChildId = childId
    }


-- | Real BD CLI integration tests.
--
-- These tests run against the actual bd CLI, creating an isolated
-- beads database for each test. Tests are skipped if bd is not available.
realBdTests :: Spec
realBdTests = do
  describe "with real bd CLI" $ do
    it "bdShow returns bead info for existing bead" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        -- bd --db expects the SQLite file path, not directory
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        result <- bdShow config fixture.tfParentId
        case result of
          Nothing -> expectationFailure $ "Expected bead, got Nothing for ID: " ++ T.unpack fixture.tfParentId
          Just bead -> do
            bead.biId `shouldBe` fixture.tfParentId
            bead.biTitle `shouldBe` "Parent Epic"
            bead.biType `shouldBe` TypeEpic
            bead.biPriority `shouldBe` 1
            bead.biStatus `shouldBe` StatusOpen

    it "bdShow returns Nothing for non-existent bead" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        result <- bdShow config "nonexistent-999"
        result `shouldBe` Nothing

    it "bdDeps returns dependencies" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        -- child depends on parent
        result <- bdDeps config fixture.tfChildId
        length result `shouldBe` 1
        case result of
          [dep] -> dep.biId `shouldBe` fixture.tfParentId
          _ -> expectationFailure "Expected exactly one dependency"

    it "bdBlocking returns blocking beads" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        -- parent is blocking child
        result <- bdBlocking config fixture.tfParentId
        length result `shouldBe` 1
        case result of
          [blocked] -> blocked.biId `shouldBe` fixture.tfChildId
          _ -> expectationFailure "Expected exactly one blocked bead"

    it "bdLabels returns labels" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        result <- bdLabels config fixture.tfParentId
        result `shouldContain` ["infrastructure"]
        result `shouldContain` ["urgent"]

    it "bdLabels returns empty for bead with no labels" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        result <- bdLabels config fixture.tfChildId
        result `shouldBe` []

  -- Write operation tests
  describe "write operations with real bd CLI" $ do
    it "bdCreate creates a new bead and returns ID" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        let input = CreateBeadInput
              { cbiTitle = "Test Created Bead"
              , cbiDescription = Just "A test description"
              , cbiType = TypeTask
              , cbiPriority = 3
              , cbiParent = Nothing
              , cbiLabels = []
              , cbiAssignee = Nothing
              , cbiDeps = []
              }
        newId <- bdCreate config input
        newId `shouldSatisfy` (not . T.null)
        -- Verify we can read it back
        result <- bdShow config newId
        case result of
          Nothing -> expectationFailure "Created bead not found"
          Just bead -> do
            bead.biTitle `shouldBe` "Test Created Bead"
            bead.biType `shouldBe` TypeTask
            bead.biPriority `shouldBe` 3

    it "bdCreate creates child under parent" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        let input = CreateBeadInput
              { cbiTitle = "Child of Parent"
              , cbiDescription = Nothing
              , cbiType = TypeTask
              , cbiPriority = 2
              , cbiParent = Just fixture.tfParentId
              , cbiLabels = []
              , cbiAssignee = Nothing
              , cbiDeps = []
              }
        childId <- bdCreate config input
        -- Verify parent relationship
        result <- bdShow config childId
        case result of
          Nothing -> expectationFailure "Created child bead not found"
          Just bead -> bead.biParent `shouldBe` Just fixture.tfParentId

    it "bdClose and bdReopen toggle bead status" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        -- Initially open
        result1 <- bdShow config fixture.tfChildId
        case result1 of
          Just bead -> bead.biStatus `shouldBe` StatusOpen
          Nothing -> expectationFailure "Bead not found"
        -- Close it
        bdClose config fixture.tfChildId
        result2 <- bdShow config fixture.tfChildId
        case result2 of
          Just bead -> bead.biStatus `shouldBe` StatusClosed
          Nothing -> expectationFailure "Bead not found after close"
        -- Reopen it
        bdReopen config fixture.tfChildId
        result3 <- bdShow config fixture.tfChildId
        case result3 of
          Just bead -> bead.biStatus `shouldBe` StatusOpen
          Nothing -> expectationFailure "Bead not found after reopen"

    it "bdAddLabel and bdRemoveLabel manage labels" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        -- Child starts with no labels
        labels1 <- bdLabels config fixture.tfChildId
        labels1 `shouldBe` []
        -- Add a label
        bdAddLabel config fixture.tfChildId "test-label"
        labels2 <- bdLabels config fixture.tfChildId
        labels2 `shouldContain` ["test-label"]
        -- Remove it
        bdRemoveLabel config fixture.tfChildId "test-label"
        labels3 <- bdLabels config fixture.tfChildId
        labels3 `shouldNotContain` ["test-label"]

    it "bdChildren returns child beads" $ do
      bdAvailable <- isBdAvailable
      when (not bdAvailable) $ pendingWith "bd CLI not available"
      withTestBeadsDb $ \fixture -> do
        let config = BDConfig { bcBeadsDir = Just (fixture.tfTmpDir </> ".beads" </> "beads.db"), bcQuiet = True }
        -- Create an actual child (with --parent, not --deps)
        let childInput = CreateBeadInput
              { cbiTitle = "Actual Child"
              , cbiDescription = Nothing
              , cbiType = TypeTask
              , cbiPriority = 2
              , cbiParent = Just fixture.tfParentId
              , cbiLabels = []
              , cbiAssignee = Nothing
              , cbiDeps = []
              }
        actualChildId <- bdCreate config childInput
        -- Now query children
        children <- bdChildren config fixture.tfParentId
        length children `shouldSatisfy` (>= 1)
        -- Our child should be in the list
        let childIds = map (\b -> b.biId) children
        childIds `shouldContain` [actualChildId]
