module Main where

import Test.Hspec
import qualified MailboxSpec
import qualified SpawnSpec
import qualified RuntimeSpec
import qualified IntegrationSpec
import qualified GraphSpec
import qualified SubgraphSpec

main :: IO ()
main = hspec $ do
  describe "Mailbox" MailboxSpec.spec
  describe "Spawn" SpawnSpec.spec
  describe "Runtime" RuntimeSpec.spec
  describe "Integration" IntegrationSpec.spec
  describe "Graph" GraphSpec.spec
  describe "Subgraph" SubgraphSpec.spec
