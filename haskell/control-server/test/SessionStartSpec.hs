module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (isJust)
import qualified Data.Text as T
import Control.Monad.Freer (runM)

import Tidepool.Control.Hook.SessionStart
import Tidepool.Control.RoleConfig (Role(..))
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "SessionStart"
  [ testGroup "Role Detection"
    [ testCase "Dev role returns context" $ do
        ctx <- runSessionStart Dev "/tmp"
        assertBool "Should return context for Dev" (isJust ctx)
        assertBool "Should contain 'Development Session' or 'Task:'" 
          (any (`T.isInfixOf` fromJust ctx) ["Development Session", "Task:"])

    , testCase "TL role returns context" $ do
        ctx <- runSessionStart TL "/tmp"
        assertBool "Should return context for TL" (isJust ctx)
        assertBool "Should contain 'Team Lead Session'" 
          ("Team Lead Session" `T.isInfixOf` fromJust ctx)

    , testCase "PM role returns Nothing" $ do
        ctx <- runSessionStart PM "/tmp"
        assertEqual "Should return Nothing for PM" Nothing ctx
    ]
  ]

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust: Nothing"

runSessionStart :: Role -> T.Text -> IO (Maybe T.Text)
runSessionStart role cwd = runM
  $ runLog Error
  $ runBDIO defaultBDConfig
  $ runGitIO
  $ sessionStartLogic role cwd
