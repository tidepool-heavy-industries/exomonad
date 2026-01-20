{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad.Freer (Eff, runM, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef

import Tidepool.Effects.BD
import Tidepool.Control.MailboxTools
import Tidepool.Graph.Goto (unwrapSingleChoice)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MailboxTools Logic"
  [ testCase "sendMessageLogic creates bead with correct labels" test_sendMessage
  , testCase "checkInboxLogic filters by role and status" test_checkInbox
  , testCase "markReadLogic closes bead with reason" test_markRead
  ]

-- | Mock BD state
data MockState = MockState
  { createdBeads :: [CreateBeadInput]
  , closedBeads  :: [(Text, Maybe Text)]
  }

emptyMockState :: MockState
emptyMockState = MockState [] []

-- | Run with mock BD
runMockBD :: IORef MockState -> [BeadInfo] -> Eff '[BD, IO] a -> IO a
runMockBD stateRef inboxBeads = runM . interpret (\case
  CreateBead input -> do
    sendM $ modifyIORef stateRef (\s -> s { createdBeads = input : createdBeads s })
    pure "msg-123"
  ListBeads input -> do
    -- Simple filtering for mock
    let filtered = filter (matches input) inboxBeads
    pure filtered
  CloseBead bid reason -> do
    sendM $ modifyIORef stateRef (\s -> s { closedBeads = (bid, reason) : closedBeads s })
    pure ()
  GetBead bid -> do
    pure $ findById bid inboxBeads
  AddDep _ _ _ -> pure ()
  _ -> error "Not implemented in mock"
  )
  where
    findById bid = foldr (\b acc -> if b.biId == bid then Just b else acc) Nothing
    matches input b = 
      let statusMatch = maybe True (== b.biStatus) input.lbiStatus
          labelMatch = all (`elem` b.biLabels) input.lbiLabels
      in statusMatch && labelMatch

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
  _ <- runMockBD stateRef [] $ unwrapSingleChoice <$> sendMessageLogic "pm" req
  
  state <- readIORef stateRef
  assertEqual "Should have created one bead" 1 (length $ createdBeads state)
  case createdBeads state of
    [created] -> do
      assertEqual "Title should match subject" "Hello" created.cbiTitle
      assertBool "Should have mailbox label" ("mailbox" `elem` created.cbiLabels)
      assertBool "Should have from:pm label" ("from:pm" `elem` created.cbiLabels)
      assertBool "Should have to:tl label" ("to:tl" `elem` created.cbiLabels)
      assertBool "Should have msgtype:proposal label" ("msgtype:proposal" `elem` created.cbiLabels)
      assertEqual "Assignee should be recipient" (Just "tl") created.cbiAssignee
    _ -> assertFailure "Expected exactly one created bead"

test_checkInbox :: Assertion
test_checkInbox = do
  stateRef <- newIORef emptyMockState
  let msg1 = (defaultBead "msg-1") { biLabels = ["mailbox", "to:tl", "from:pm"], biStatus = StatusOpen }
  let msg2 = (defaultBead "msg-2") { biLabels = ["mailbox", "to:pm", "from:tl"], biStatus = StatusOpen }
  
  -- Check TL inbox
  results <- runMockBD stateRef [msg1, msg2] $ unwrapSingleChoice <$> checkInboxLogic "tl" (CheckInboxArgs)
  assertEqual "TL should see 1 message" 1 (length results)
  case results of
    (msg:_) -> assertEqual "TL should see msg-1" "msg-1" msg.msId
    [] -> assertFailure "Expected TL to see at least one message"
  
  -- Check PM inbox
  results2 <- runMockBD stateRef [msg1, msg2] $ unwrapSingleChoice <$> checkInboxLogic "pm" (CheckInboxArgs)
  assertEqual "PM should see 1 message" 1 (length results2)
  case results2 of
    (msg:_) -> assertEqual "PM should see msg-2" "msg-2" msg.msId
    [] -> assertFailure "Expected PM to see at least one message"

test_markRead :: Assertion
test_markRead = do
  stateRef <- newIORef emptyMockState
  _ <- runMockBD stateRef [] $ unwrapSingleChoice <$> markReadLogic (MarkReadArgs "msg-1" (Just "acknowledged"))
  
  state <- readIORef stateRef
  assertEqual "Should have closed one bead" 1 (length $ closedBeads state)
  case closedBeads state of
    [(bid, reason)] -> do
      assertEqual "Should close correct ID" "msg-1" bid
      assertEqual "Should have correct reason" (Just "acknowledged") reason
    _ -> assertFailure "Expected exactly one closed bead"

-- Helper to create dummy bead
defaultBead :: Text -> BeadInfo
defaultBead bid = BeadInfo
  { biId = bid
  , biTitle = ""
  , biDescription = Nothing
  , biStatus = StatusOpen
  , biPriority = 2
  , biType = TypeTask
  , biAssignee = Nothing
  , biCreatedAt = Nothing
  , biCreatedBy = Nothing
  , biUpdatedAt = Nothing
  , biParent = Nothing
  , biLabels = []
  , biDependencies = []
  , biDependents = []
  }