{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.Freer (Eff, runM, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef

import ExoMonad.Effects.GitHub
import ExoMonad.Control.MailboxTools
import ExoMonad.Graph.Goto (unwrapSingleChoice)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MailboxTools Logic"
  [ testCase "sendMessageLogic creates issue with correct labels" test_sendMessage
  , testCase "checkInboxLogic filters by role and status" test_checkInbox
  , testCase "markReadLogic closes issue" test_markRead
  ]

-- | Mock GitHub state
data MockState = MockState
  { createdIssues :: [CreateIssueInput]
  , closedIssues  :: [Int]
  }

emptyMockState :: MockState
emptyMockState = MockState [] []

-- | Run with mock GitHub
runMockGitHub :: IORef MockState -> [Issue] -> Eff '[GitHub, IO] a -> IO a
runMockGitHub stateRef inboxIssues = runM . interpret (\case
  CreateIssue input -> do
    sendM $ modifyIORef stateRef (\s -> s { createdIssues = input : createdIssues s })
    pure 123
  ListIssues _ _ -> do
    pure inboxIssues
  GetIssue _ num _ -> do
    pure $ findByNum num inboxIssues
  CloseIssue _ num -> do
    sendM $ modifyIORef stateRef (\s -> s { closedIssues = num : closedIssues s })
    pure ()
  _ -> error "Not implemented in mock"
  )
  where
    findByNum num = foldr (\i acc -> if i.issueNumber == num then Just i else acc) Nothing

test_sendMessage :: Assertion
test_sendMessage = do
  stateRef <- newIORef emptyMockState
  let req = SendRequest 
        { to = "tl"
        , msgType = "proposal"
        , subject = "Hello"
        , body = "World"
        , blockWork = Nothing
        }
  _ <- runMockGitHub stateRef [] $ unwrapSingleChoice <$> sendMessageLogic "pm" req
  
  state <- readIORef stateRef
  assertEqual "Should have created one issue" 1 (length $ createdIssues state)
  case createdIssues state of
    [created] -> do
      assertEqual "Title should match subject" "Hello" created.ciiTitle
      assertBool "Should have mailbox label" ("mailbox" `elem` created.ciiLabels)
      assertBool "Should have from:pm label" ("from:pm" `elem` created.ciiLabels)
      assertBool "Should have to:tl label" ("to:tl" `elem` created.ciiLabels)
      assertBool "Should have msgtype:proposal label" ("msgtype:proposal" `elem` created.ciiLabels)
      assertEqual "Assignee should be recipient" ["tl"] created.ciiAssignees
    _ -> assertFailure "Expected exactly one created issue"

test_checkInbox :: Assertion
test_checkInbox = do
  stateRef <- newIORef emptyMockState
  let msg1 = (defaultIssue 1) { issueLabels = ["mailbox", "to:tl", "from:pm"], issueState = IssueOpen, issueTitle = "Msg 1" }
  let msg2 = (defaultIssue 2) { issueLabels = ["mailbox", "to:pm", "from:tl"], issueState = IssueOpen, issueTitle = "Msg 2" }
  
  -- Check TL inbox
  results <- runMockGitHub stateRef [msg1] $ unwrapSingleChoice <$> checkInboxLogic "tl" (CheckInboxArgs)
  assertEqual "TL should see 1 message" 1 (length results)
  case results of
    (msg:_) -> assertEqual "TL should see msg-1" 1 msg.msId
    [] -> assertFailure "Expected TL to see at least one message"
  
  -- Check PM inbox
  results2 <- runMockGitHub stateRef [msg2] $ unwrapSingleChoice <$> checkInboxLogic "pm" (CheckInboxArgs)
  assertEqual "PM should see 1 message" 1 (length results2)
  case results2 of
    (msg:_) -> assertEqual "PM should see msg-2" 2 msg.msId
    [] -> assertFailure "Expected PM to see at least one message"

test_markRead :: Assertion
test_markRead = do
  stateRef <- newIORef emptyMockState
  _ <- runMockGitHub stateRef [] $ unwrapSingleChoice <$> markReadLogic (MarkReadArgs 123 (Just "acknowledged"))
  
  state <- readIORef stateRef
  assertEqual "Should have closed one issue" 1 (length $ closedIssues state)
  case closedIssues state of
    [num] -> do
      assertEqual "Should close correct number" 123 num
    _ -> assertFailure "Expected exactly one closed issue"

-- Helper to create dummy issue
defaultIssue :: Int -> Issue
defaultIssue num = Issue
  { issueNumber = num
  , issueTitle = ""
  , issueBody = ""
  , issueState = IssueOpen
  , issueLabels = []
  , issueAuthor = Author "ghost" Nothing
  , issueUrl = ""
  }
