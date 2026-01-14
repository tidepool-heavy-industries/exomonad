-- | BD interpreter integration tests.
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
import Tidepool.Effects.Git (WorktreeInfo(..))
import Tidepool.BD.Interpreter
import Tidepool.BD.Prime.Graph (PrimeContext(..), renderPrime, renderPrimeJSON)
import Tidepool.BD.GitInterpreter (worktreeName)


main :: IO ()
main = hspec $ do
  describe "BD Types - JSON Parsing" $ do
    beadStatusTests
    beadTypeTests
    dependencyTypeTests
    dependencyInfoTests
    beadInfoTests

  describe "BD Interpreter - Mock CLI" $ do
    interpreterTests

  describe "BD Interpreter - Real CLI" $ do
    realBdTests

  describe "Urchin Prime - Worktree Detection" $ do
    worktreeNameTests

  describe "Urchin Prime - Context Rendering" $ do
    primeRenderingTests


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
-- INTERPRETER TESTS
-- ════════════════════════════════════════════════════════════════════════════

interpreterTests :: Spec
interpreterTests = describe "Interpreter CLI Integration" $ do
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
    let mockListByStatus _ = pure []
    let mockListByType _ = pure []

    result <- runM $ runBD mockGetBead mockGetDeps mockGetBlocking mockGetLabels mockListByStatus mockListByType $ do
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


-- ════════════════════════════════════════════════════════════════════════════
-- WORKTREE NAME TESTS
-- ════════════════════════════════════════════════════════════════════════════

worktreeNameTests :: Spec
worktreeNameTests = describe "worktreeName" $ do
  it "extracts name from worktrees path" $ do
    worktreeName "/home/user/project/worktrees/bd" `shouldBe` "bd"

  it "extracts name from nested worktrees path" $ do
    worktreeName "/home/inanna/dev/tidepool/worktrees/native-server" `shouldBe` "native-server"

  it "falls back to last component for non-worktree path" $ do
    worktreeName "/home/user/some-project" `shouldBe` "some-project"

  it "handles trailing slash" $ do
    worktreeName "/home/user/project/worktrees/bd/" `shouldBe` "bd"

  it "extracts from path with multiple worktrees directories" $ do
    -- Takes the component after the first "worktrees"
    worktreeName "/worktrees/old/worktrees/new-feature" `shouldBe` "old"


-- ════════════════════════════════════════════════════════════════════════════
-- PRIME RENDERING TESTS
-- ════════════════════════════════════════════════════════════════════════════

primeRenderingTests :: Spec
primeRenderingTests = describe "renderPrime" $ do
  it "renders header with worktree and branch" $ do
    let ctx = minimalContext
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "# Urchin Prime: test-wt / main"

  it "renders git section with branch and dirty count" $ do
    let ctx = minimalContext { pcDirtyFiles = ["file1.hs", "file2.hs"] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "**Branch**: main"
    output `shouldSatisfy` T.isInfixOf "**Dirty files**: 2"

  it "renders recent commit if present" $ do
    let ctx = minimalContext { pcRecentCommits = ["Initial commit", "Second commit"] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "**Recent**: Initial commit"

  it "renders propulsion section when hooked work exists" $ do
    let hookedBead = testBead { biId = "hooked-1", biTitle = "Urgent work", biStatus = StatusHooked }
    let ctx = minimalContext { pcHooked = [hookedBead] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "## Propulsion"
    output `shouldSatisfy` T.isInfixOf "**HOOKED WORK**"
    output `shouldSatisfy` T.isInfixOf "[hooked-1] Urgent work"

  it "renders no propulsion message when no hooked work" $ do
    let ctx = minimalContext { pcHooked = [] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "No hooked work"

  it "renders in-progress beads" $ do
    let inProgressBead = testBead { biId = "wip-1", biTitle = "Working on it", biPriority = 1 }
    let ctx = minimalContext { pcInProgress = [inProgressBead] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "## In Progress (1)"
    output `shouldSatisfy` T.isInfixOf "[wip-1] Working on it (P1)"

  it "renders ready beads" $ do
    let readyBead = testBead { biId = "ready-1", biTitle = "Ready to go", biPriority = 2 }
    let ctx = minimalContext { pcReady = [readyBead] }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "## Ready (1)"
    output `shouldSatisfy` T.isInfixOf "[ready-1] Ready to go (P2)"

  it "renders epic context when present" $ do
    let epicBead = testBead { biId = "epic-1", biTitle = "Big Feature Epic", biType = TypeEpic }
    let ctx = minimalContext { pcEpicContext = Just epicBead }
    let output = renderPrime ctx
    output `shouldSatisfy` T.isInfixOf "## Epic Context"
    output `shouldSatisfy` T.isInfixOf "**epic-1**: Big Feature Epic"

  it "omits epic section when no epic" $ do
    let ctx = minimalContext { pcEpicContext = Nothing }
    let output = renderPrime ctx
    output `shouldSatisfy` (not . T.isInfixOf "## Epic Context")

  it "renderPrimeJSON produces valid JSON" $ do
    let ctx = minimalContext
    let jsonOutput = renderPrimeJSON ctx
    jsonOutput `shouldSatisfy` T.isPrefixOf "{"
    jsonOutput `shouldSatisfy` T.isInfixOf "\"worktree\""
    jsonOutput `shouldSatisfy` T.isInfixOf "\"git\""
    jsonOutput `shouldSatisfy` T.isInfixOf "\"in_progress\""


-- | Minimal context for testing.
minimalContext :: PrimeContext
minimalContext = PrimeContext
  { pcWorktree = WorktreeInfo
      { wiName = "test-wt"
      , wiPath = "/tmp/test"
      , wiBranch = "main"
      , wiRepoRoot = "/tmp/test"
      , wiIsWorktree = False
      }
  , pcDirtyFiles = []
  , pcRecentCommits = []
  , pcInProgress = []
  , pcReady = []
  , pcHooked = []
  , pcEpicContext = Nothing
  }


-- | Test bead template.
testBead :: BeadInfo
testBead = BeadInfo
  { biId = "test-1"
  , biTitle = "Test"
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
