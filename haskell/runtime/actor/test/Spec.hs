module Main where

import GraphSpec qualified
import IntegrationSpec qualified
import MailboxSpec qualified
import RuntimeSpec qualified
import SpawnSpec qualified
import SubgraphSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Mailbox" MailboxSpec.spec
  describe "Spawn" SpawnSpec.spec
  describe "Runtime" RuntimeSpec.spec
  describe "Integration" IntegrationSpec.spec
  describe "Graph" GraphSpec.spec
  describe "Subgraph" SubgraphSpec.spec
