{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft, isRight)
import ExoMonad.Guest.Parsers
  ( MCPCallOutput (..),
    errorResult,
    parseCleanupAgentsArgs,
    parseGetIssueArgs,
    parseGitLogArgs,
    parseGitPathArgs,
    parseListIssuesArgs,
    parseListPRsArgs,
    parseReadFileArgs,
    parseSpawnAgentsArgs,
    successResult,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSpawnAgentsArgs" $ do
    it "parses valid input with all fields" $ do
      let input =
            object
              [ "issues" .= (["1", "2"] :: [String]),
                "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String),
                "worktree_dir" .= ("/tmp" :: String)
              ]
      parseEither parseSpawnAgentsArgs input `shouldSatisfy` isRight

    it "parses without optional worktree_dir" $ do
      let input =
            object
              [ "issues" .= (["1"] :: [String]),
                "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseSpawnAgentsArgs input `shouldSatisfy` isRight

    it "fails without owner" $ do
      let input =
            object
              [ "issues" .= (["1"] :: [String]),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseSpawnAgentsArgs input `shouldSatisfy` isLeft

    it "fails without repo" $ do
      let input =
            object
              [ "issues" .= (["1"] :: [String]),
                "owner" .= ("foo" :: String)
              ]
      parseEither parseSpawnAgentsArgs input `shouldSatisfy` isLeft

    it "fails without issues" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseSpawnAgentsArgs input `shouldSatisfy` isLeft

  describe "parseCleanupAgentsArgs" $ do
    it "defaults force to False" $ do
      let input = object ["issues" .= (["1"] :: [String])]
      case parseEither parseCleanupAgentsArgs input of
        Right (_, force) -> force `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "parses force=true" $ do
      let input =
            object
              [ "issues" .= (["1"] :: [String]),
                "force" .= True
              ]
      case parseEither parseCleanupAgentsArgs input of
        Right (_, force) -> force `shouldBe` True
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "parses force=false explicitly" $ do
      let input =
            object
              [ "issues" .= (["1"] :: [String]),
                "force" .= False
              ]
      case parseEither parseCleanupAgentsArgs input of
        Right (_, force) -> force `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "fails without issues" $ do
      let input = object ["force" .= True]
      parseEither parseCleanupAgentsArgs input `shouldSatisfy` isLeft

  describe "parseGitLogArgs" $ do
    it "defaults limit to 10" $ do
      let input = object []
      case parseEither parseGitLogArgs input of
        Right (_, limit) -> limit `shouldBe` 10
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "accepts custom limit" $ do
      let input = object ["limit" .= (5 :: Int)]
      case parseEither parseGitLogArgs input of
        Right (_, limit) -> limit `shouldBe` 5
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "accepts optional path" $ do
      let input = object ["path" .= ("/some/dir" :: String), "limit" .= (20 :: Int)]
      case parseEither parseGitLogArgs input of
        Right (path, limit) -> do
          path `shouldBe` Just "/some/dir"
          limit `shouldBe` 20
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "parseGitPathArgs" $ do
    it "parses empty object as Nothing" $ do
      let input = object []
      parseEither parseGitPathArgs input `shouldBe` Right Nothing

    it "parses path when provided" $ do
      let input = object ["path" .= ("/some/dir" :: String)]
      parseEither parseGitPathArgs input `shouldBe` Right (Just "/some/dir")

  describe "parseReadFileArgs" $ do
    it "requires path field" $ do
      let input = object []
      parseEither parseReadFileArgs input `shouldSatisfy` isLeft

    it "parses path with default max_bytes" $ do
      let input = object ["path" .= ("/file.txt" :: String)]
      case parseEither parseReadFileArgs input of
        Right (path, maxBytes) -> do
          path `shouldBe` "/file.txt"
          maxBytes `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "parses path with custom max_bytes" $ do
      let input =
            object
              [ "path" .= ("/file.txt" :: String),
                "max_bytes" .= (1024 :: Int)
              ]
      case parseEither parseReadFileArgs input of
        Right (path, maxBytes) -> do
          path `shouldBe` "/file.txt"
          maxBytes `shouldBe` 1024
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "parseListIssuesArgs" $ do
    it "requires owner and repo" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseListIssuesArgs input `shouldSatisfy` isRight

    it "fails without owner" $ do
      let input = object ["repo" .= ("bar" :: String)]
      parseEither parseListIssuesArgs input `shouldSatisfy` isLeft

    it "fails without repo" $ do
      let input = object ["owner" .= ("foo" :: String)]
      parseEither parseListIssuesArgs input `shouldSatisfy` isLeft

    it "accepts optional state and labels" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String),
                "state" .= ("closed" :: String),
                "labels" .= (["bug", "urgent"] :: [String])
              ]
      parseEither parseListIssuesArgs input `shouldSatisfy` isRight

  describe "parseGetIssueArgs" $ do
    it "requires owner, repo, and number" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String),
                "number" .= (42 :: Int)
              ]
      parseEither parseGetIssueArgs input `shouldSatisfy` isRight

    it "fails without number" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseGetIssueArgs input `shouldSatisfy` isLeft

    it "fails without owner" $ do
      let input =
            object
              [ "repo" .= ("bar" :: String),
                "number" .= (42 :: Int)
              ]
      parseEither parseGetIssueArgs input `shouldSatisfy` isLeft

  describe "parseListPRsArgs" $ do
    it "requires owner and repo" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String)
              ]
      parseEither parseListPRsArgs input `shouldSatisfy` isRight

    it "accepts optional state and limit" $ do
      let input =
            object
              [ "owner" .= ("foo" :: String),
                "repo" .= ("bar" :: String),
                "state" .= ("all" :: String),
                "limit" .= (50 :: Int)
              ]
      parseEither parseListPRsArgs input `shouldSatisfy` isRight

  describe "successResult" $ do
    it "has success=True" $ do
      let r = successResult (object ["ok" .= True])
      success r `shouldBe` True

    it "has result set" $ do
      let r = successResult (object ["ok" .= True])
      result r `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "has no error" $ do
      let r = successResult (object ["ok" .= True])
      mcpError r `shouldBe` Nothing

  describe "errorResult" $ do
    it "has success=False" $ do
      let r = errorResult "something broke"
      success r `shouldBe` False

    it "has no result" $ do
      let r = errorResult "something broke"
      result r `shouldBe` Nothing

    it "has error message" $ do
      let r = errorResult "something broke"
      mcpError r `shouldBe` Just "something broke"
