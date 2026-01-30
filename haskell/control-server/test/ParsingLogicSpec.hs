{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Import the functions to test
import ExoMonad.Control.ExoTools.Internal (parseIssueNumber)
import ExoMonad.Control.Role.Schema (camelToSnake)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Parsing Logic Tests"
    [ testGroup
        "parseIssueNumber"
        [ testCase "Valid branch: gh-397/popup-tool" $
            parseIssueNumber "gh-397/popup-tool" @?= Just 397,
          testCase "Valid branch: gh-1/initial" $
            parseIssueNumber "gh-1/initial" @?= Just 1,
          testCase "Valid branch: gh-397-popup-tool (hyphen separator)" $
            parseIssueNumber "gh-397-popup-tool" @?= Just 397,
          testCase "Valid branch: gh-397 (no separator)" $
            parseIssueNumber "gh-397" @?= Just 397,
          testCase "Valid branch: GH-397/upper-case-prefix" $
            parseIssueNumber "GH-397/upper-case-prefix" @?= Just 397,
          testCase "Valid branch: gh-397/" $
            parseIssueNumber "gh-397/" @?= Just 397,
          testCase "Invalid branch: feature/foo" $
            parseIssueNumber "feature/foo" @?= Nothing,
          testCase "Invalid branch: main" $
            parseIssueNumber "main" @?= Nothing,
          testCase "Invalid branch: gh-/no-number" $
            parseIssueNumber "gh-/no-number" @?= Nothing,
          testCase "Invalid branch: gh-abc/no-number" $
            parseIssueNumber "gh-abc/no-number" @?= Nothing
        ],
      testGroup
        "camelToSnake"
        [ testCase "Simple CamelCase: filePR" $
            camelToSnake "filePR" @?= "file_pr",
          testCase "Simple CamelCase: FilePR" $
            camelToSnake "FilePR" @?= "file_pr",
          testCase "Simple CamelCase: myTool" $
            camelToSnake "myTool" @?= "my_tool",
          testCase "Already SnakeCase: simple" $
            camelToSnake "simple" @?= "simple",
          testCase "Acronyms: HTMLParser" $
            camelToSnake "HTMLParser" @?= "html_parser",
          testCase "Acronyms: MakeXMLRequest" $
            camelToSnake "MakeXMLRequest" @?= "make_xml_request",
          testCase "Acronyms: glTF2" $
            camelToSnake "glTF2" @?= "gl_tf2",
          testCase "Single letter: A" $
            camelToSnake "A" @?= "a",
          testCase "Two letters: AB" $
            camelToSnake "AB" @?= "ab",
          testCase "Three letters: ABC" $
            camelToSnake "ABC" @?= "abc"
        ]
    ]
