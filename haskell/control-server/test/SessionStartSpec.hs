module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (isJust)
import qualified Data.Text as T
import Control.Monad.Freer (Eff, runM, interpret)

import Tidepool.Control.Hook.SessionStart
import Tidepool.Control.RoleConfig (Role(..))
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Effects.Git (Git(..))
import Tidepool.Effects.GitHub (GitHub(..), Repo(..), Issue(..), IssueState(..), Author(..))
import Tidepool.Git.Interpreter (runGitIO)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "SessionStart"
  [ testGroup "Role Detection"
    [ testCase "Dev role returns context" $ do
        ctx <- runSessionStart Dev "/tmp"
        assertBool "Should return context for Dev" (isJust ctx)
        -- The template might mention 'Development Session' or 'Task:'
        let rendered = fromJust ctx
        assertBool ("Should contain 'Development Session' or 'Task:', got: " <> T.unpack rendered)
          (any (`T.isInfixOf` rendered) ["Development Session", "Task:"])

    , testCase "TL role returns context" $ do
        ctx <- runSessionStart TL "/tmp"
        assertBool "Should return context for TL" (isJust ctx)
        let rendered = fromJust ctx
        assertBool ("Should contain 'Team Lead Session', got: " <> T.unpack rendered)
          ("Team Lead Session" `T.isInfixOf` rendered)

    , testCase "PM role returns Nothing" $ do
        ctx <- runSessionStart PM "/tmp"
        assertEqual "Should return Nothing for PM" Nothing ctx
    ]
  ]

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust: Nothing"

-- | Mock GitHub interpreter
runMockGitHub :: Eff (GitHub ': effs) a -> Eff effs a
runMockGitHub = interpret $ \case
  GetIssue _ _ _ -> pure Nothing
  ListIssues _ _ -> pure []
  CreateIssue _ _ _ -> pure 1
  UpdateIssue _ _ _ -> pure 1
  CloseIssue _ _ -> pure 1
  ReopenIssue _ _ -> pure 1
  AddIssueLabel _ _ _ -> pure ()
  RemoveIssueLabel _ _ _ -> pure ()
  GetRepo _ -> pure $ Repo "tidepool/tidepool"

runSessionStart :: Role -> T.Text -> IO (Maybe T.Text)
runSessionStart role cwd = runM
  $ runLog Error
  $ runMockGitHub
  $ runGitIO
  $ sessionStartLogic role cwd
