{-# LANGUAGE OverloadedStrings #-}

module ToolsSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isAsciiLower)
import Data.List (nub)
import qualified Data.Text as T
import ExoMonad.Guest.Tools
import Test.Hspec

spec :: Spec
spec = do
  describe "allTools" $ do
    it "exports exactly 10 tools" $
      length allTools `shouldBe` 10

    it "has no duplicate tool names" $ do
      let names = map tdName allTools
      nub names `shouldBe` names

    it "all names are lowercase_snake_case" $
      all (T.all isSnakeCaseChar . tdName) allTools `shouldBe` True

    it "all descriptions are non-empty" $
      not (any (T.null . tdDescription) allTools) `shouldBe` True

  describe "toMCPFormat" $ do
    it "produces object with name, description, inputSchema" $ do
      case allTools of
        (tool:_) -> do
          let json = toMCPFormat tool
          case json of
            Object obj -> do
              KM.member "name" obj `shouldBe` True
              KM.member "description" obj `shouldBe` True
              KM.member "inputSchema" obj `shouldBe` True
            _ -> expectationFailure "Expected JSON object"
        [] -> expectationFailure "allTools is empty"

    it "name field matches tdName" $ do
      case allTools of
        (tool:_) -> do
          let json = toMCPFormat tool
          case json of
            Object obj ->
              KM.lookup "name" obj `shouldBe` Just (String (tdName tool))
            _ -> expectationFailure "Expected JSON object"
        [] -> expectationFailure "allTools is empty"

  describe "schema structure" $ do
    it "all schemas have type: object" $ do
      mapM_
        ( \tool ->
            case tdInputSchema tool of
              Object obj ->
                KM.lookup "type" obj `shouldBe` Just (String "object")
              _ ->
                expectationFailure $
                  "Schema not object: " ++ T.unpack (tdName tool)
        )
        allTools

    it "all schemas have properties field" $ do
      mapM_
        ( \tool ->
            case tdInputSchema tool of
              Object obj ->
                KM.member "properties" obj `shouldBe` True
              _ ->
                expectationFailure "Schema not object"
        )
        allTools

  describe "tool inventory" $ do
    let toolNames = map tdName allTools
    it "contains git_branch" $
      "git_branch" `elem` toolNames `shouldBe` True
    it "contains git_status" $
      "git_status" `elem` toolNames `shouldBe` True
    it "contains git_log" $
      "git_log" `elem` toolNames `shouldBe` True
    it "contains read_file" $
      "read_file" `elem` toolNames `shouldBe` True
    it "contains github_list_issues" $
      "github_list_issues" `elem` toolNames `shouldBe` True
    it "contains github_get_issue" $
      "github_get_issue" `elem` toolNames `shouldBe` True
    it "contains github_list_prs" $
      "github_list_prs" `elem` toolNames `shouldBe` True
    it "contains spawn_agents" $
      "spawn_agents" `elem` toolNames `shouldBe` True
    it "contains cleanup_agents" $
      "cleanup_agents" `elem` toolNames `shouldBe` True
    it "contains list_agents" $
      "list_agents" `elem` toolNames `shouldBe` True

  describe "required fields" $ do
    it "read_file requires path" $ do
      let Just tool = findTool "read_file"
      hasRequired "path" (tdInputSchema tool) `shouldBe` True

    it "github_list_issues requires owner and repo" $ do
      let Just tool = findTool "github_list_issues"
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True

    it "github_get_issue requires owner, repo, and number" $ do
      let Just tool = findTool "github_get_issue"
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True
      hasRequired "number" (tdInputSchema tool) `shouldBe` True

    it "spawn_agents requires issues, owner, and repo" $ do
      let Just tool = findTool "spawn_agents"
      hasRequired "issues" (tdInputSchema tool) `shouldBe` True
      hasRequired "owner" (tdInputSchema tool) `shouldBe` True
      hasRequired "repo" (tdInputSchema tool) `shouldBe` True

    it "cleanup_agents requires issues" $ do
      let Just tool = findTool "cleanup_agents"
      hasRequired "issues" (tdInputSchema tool) `shouldBe` True

-- Helper: Check if a character is valid for snake_case
isSnakeCaseChar :: Char -> Bool
isSnakeCaseChar c = c == '_' || isAsciiLower c

-- Helper: Find a tool by name
findTool :: T.Text -> Maybe ToolDefinition
findTool name = case filter ((== name) . tdName) allTools of
  [t] -> Just t
  _ -> Nothing

-- Helper: Check if a field is in the required array
hasRequired :: T.Text -> Value -> Bool
hasRequired field (Object obj) =
  case KM.lookup "required" obj of
    Just (Array arr) -> String field `elem` arr
    _ -> False
hasRequired _ _ = False
