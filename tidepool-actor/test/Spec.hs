module Main where

import Test.Hspec
import qualified MailboxSpec
import qualified SpawnSpec
import qualified RuntimeSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  describe "Mailbox" MailboxSpec.spec
  describe "Spawn" SpawnSpec.spec
  describe "Runtime" RuntimeSpec.spec
  describe "Integration" IntegrationSpec.spec
