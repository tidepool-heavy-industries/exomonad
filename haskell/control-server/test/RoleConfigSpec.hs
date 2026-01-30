module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import ExoMonad.Control.RoleConfig
import ExoMonad.Role (Role (..))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup
    "Role Configuration"
    [ testGroup
        "Tool Access"
        [ testCase "TL can spawn_agents" $
            isToolAllowed TL "spawn_agents" @?= True,
          testCase "TL cannot file_pr" $
            isToolAllowed TL "file_pr" @?= False,
          testCase "Dev can file_pr" $
            isToolAllowed Dev "file_pr" @?= True,
          testCase "Dev cannot spawn_agents" $
            isToolAllowed Dev "spawn_agents" @?= False,
          testCase "PM can pm_status" $
            isToolAllowed PM "pm_status" @?= True,
          testCase "PM cannot spawn_agents" $
            isToolAllowed PM "spawn_agents" @?= False
        ],
      testGroup
        "Role Parsing"
        [ testCase "dev parses to Dev" $
            roleFromText "dev" @?= Just Dev,
          testCase "tl parses to TL" $
            roleFromText "tl" @?= Just TL,
          testCase "pm parses to PM" $
            roleFromText "pm" @?= Just PM,
          testCase "invalid returns Nothing" $
            roleFromText "invalid" @?= Nothing
        ]
    ]
