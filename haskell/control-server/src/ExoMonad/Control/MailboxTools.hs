{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ExoMonad.Control.MailboxTools
  ( SendRequest(..)
  , MessageSummary(..)
  , MessageDetail(..)
  , sendMessageLogic
  , checkInboxLogic
  , readMessageLogic
  , markReadLogic
  
    -- * Graphs (for registration)
  , SendMessageGraph(..)
  , CheckInboxGraph(..)
  , ReadMessageGraph(..)
  , MarkReadGraph(..)
  , MarkReadArgs(..)
  , ReadMessageArgs(..)
  , CheckInboxArgs(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effects.GitHub

  ( GitHub

  , CreateIssueInput(..)

  , defaultCreateIssueInput

  , IssueFilter(..)

  , defaultIssueFilter

  , Issue(..)

  , IssueState(..)

  , createIssue

  , listIssues

  , getIssue

  , closeIssue

  , Repo(..)

  )

import ExoMonad.Graph.Generic (type (:-))

import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)

import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)

import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)

import ExoMonad.Schema (HasJSONSchema(..), objectSchema, describeField, emptySchema, SchemaType(..))



-- | Roles for routing.

type Role = Text



-- ════════════════════════════════════════════════════════════════════════════

-- SEND MESSAGE

-- ════════════════════════════════════════════════════════════════════════════



-- | Request to send a message.

data SendRequest = SendRequest

  { to        :: Role

  , msgType   :: Text

  , subject   :: Text

  , body      :: Text

  , blockWork :: Maybe Int -- Issue number

  }

  deriving stock (Show, Eq, Generic)



instance HasJSONSchema SendRequest where

  jsonSchema = objectSchema

    [ ("to", describeField "to" "Recipient role (e.g., 'pm', 'tl')" (emptySchema TString))

    , ("msgType", describeField "msgType" "Message type (proposal, question, briefing, approval)" (emptySchema TString))

    , ("subject", describeField "subject" "Message subject" (emptySchema TString))

    , ("body", describeField "body" "Markdown message content" (emptySchema TString))

    , ("blockWork", describeField "blockWork" "Optional: issue number to block (legacy, currently unused)" (emptySchema TInteger))

    ]

    ["to", "msgType", "subject", "body"]



instance FromJSON SendRequest where

  parseJSON = withObject "SendRequest" $ \v ->

    SendRequest

      <$> v .: "to"

      <*> v .: "msgType"

      <*> v .: "subject"

      <*> v .: "body"

      <*> v .:? "blockWork"



instance ToJSON SendRequest where

  toJSON req = object

    [ "to" .= req.to

    , "msgType" .= req.msgType

    , "subject" .= req.subject

    , "body" .= req.body

    , "blockWork" .= req.blockWork

    ]



-- | Graph definition for send_message.

data SendMessageGraph mode = SendMessageGraph

  { smEntry :: mode :- EntryNode SendRequest

      :@ MCPExport

      :@ MCPToolDef ('("send_message", "Send a message to another agent using GitHub issues."))



  , smRun :: mode :- LogicNode

      :@ Input SendRequest

      :@ UsesEffects '[GitHub, Goto Exit Int]



  , smExit :: mode :- ExitNode Int

  }

  deriving Generic



-- | Logic for send_message.

sendMessageLogic

  :: Member GitHub es

  => Role

  -> SendRequest

  -> Eff es (GotoChoice '[To Exit Int])

sendMessageLogic fromRole req = do

  -- TODO: Configurable repo

  let repo = Repo "exomonad/exomonad"

  let labels = ["mailbox", "from:" <> fromRole, "to:" <> req.to, "msgtype:" <> req.msgType]

  msgNum <- createIssue $ (defaultCreateIssueInput repo req.subject)

    { ciiBody      = req.body

    , ciiLabels    = labels

    , ciiAssignees = [req.to]

    }

  

  -- Dependency tracking currently not implemented for GH issues in this way

  -- case req.blockWork of

  --   Just workId -> addDep workId msgId DepDependsOn

  --   Nothing -> pure ()

    

  pure $ gotoExit msgNum



-- ════════════════════════════════════════════════════════════════════════════

-- CHECK INBOX

-- ════════════════════════════════════════════════════════════════════════════



-- | Summary of a message in the inbox.

data MessageSummary = MessageSummary

  { msId      :: Int

  , msFrom    :: Role

  , msMsgType :: Text

  , msSubject :: Text

  , msStatus  :: Text

  }

  deriving stock (Show, Eq, Generic)



instance ToJSON MessageSummary where

  toJSON m = object

    [ "id" .= m.msId

    , "from" .= m.msFrom

    , "msgType" .= m.msMsgType

    , "subject" .= m.msSubject

    , "status" .= m.msStatus

    ]



-- | Dummy args for check_inbox.

data CheckInboxArgs = CheckInboxArgs {}

  deriving stock (Show, Eq, Generic)

  deriving anyclass (ToJSON)



-- | Accept empty object {} from MCP (Generic derives Array for empty records)

instance FromJSON CheckInboxArgs where

  parseJSON = withObject "CheckInboxArgs" $ \_ -> pure CheckInboxArgs



instance HasJSONSchema CheckInboxArgs where

  jsonSchema = objectSchema [] []



-- | Graph definition for check_inbox.

data CheckInboxGraph mode = CheckInboxGraph

  { ciEntry :: mode :- EntryNode CheckInboxArgs

      :@ MCPExport

      :@ MCPToolDef ('("check_inbox", "Check current agent's message inbox."))



  , ciRun :: mode :- LogicNode

      :@ Input CheckInboxArgs

      :@ UsesEffects '[GitHub, Goto Exit [MessageSummary]]



  , ciExit :: mode :- ExitNode [MessageSummary]

  }

  deriving Generic



-- | Logic for check_inbox.

-- | Logic for check_inbox.
checkInboxLogic
  :: Member GitHub es
  => Role
  -> CheckInboxArgs
  -> Eff es (GotoChoice '[To Exit [MessageSummary]])
checkInboxLogic myRole _ = do
  -- TODO: Configurable repo
  let repo = Repo "exomonad/exomonad"
  issues <- listIssues repo defaultIssueFilter
    { ifState = Just IssueOpen
    , ifLabels = ["mailbox", "to:" <> myRole]
    }
  pure $ gotoExit $ map issueToSummary issues



toIssueBrief :: Issue -> MessageSummary

toIssueBrief = issueToSummary



issueToSummary :: Issue -> MessageSummary

issueToSummary i = MessageSummary

  { msId = i.issueNumber

  , msFrom = findLabelPrefix "from:" i.issueLabels

  , msMsgType = findLabelPrefix "msgtype:" i.issueLabels

  , msSubject = i.issueTitle

  , msStatus = T.pack $ show i.issueState

  }



findLabelPrefix :: Text -> [Text] -> Text

findLabelPrefix prefix labels =

  fromMaybe "unknown" $

    listToMaybe $

      map (T.drop (T.length prefix)) $

        filter (T.isPrefixOf prefix) labels



-- ════════════════════════════════════════════════════════════════════════════

-- READ MESSAGE

-- ════════════════════════════════════════════════════════════════════════════



-- | Detailed message content.

data MessageDetail = MessageDetail

  { mdId      :: Int

  , mdFrom    :: Role

  , mdTo      :: Role

  , mdMsgType :: Text

  , mdSubject :: Text

  , mdBody    :: Text

  , mdStatus  :: Text

  }

  deriving stock (Show, Eq, Generic)



instance ToJSON MessageDetail where

  toJSON m = object

    [ "id" .= m.mdId

    , "from" .= m.mdFrom

    , "to" .= m.mdTo

    , "msgType" .= m.mdMsgType

    , "subject" .= m.mdSubject

    , "body" .= m.mdBody

    , "status" .= m.mdStatus

    ]



data ReadMessageArgs = ReadMessageArgs

  { rmaMessageId :: Int

  }

  deriving stock (Show, Eq, Generic)



instance HasJSONSchema ReadMessageArgs where

  jsonSchema = objectSchema

    [ ("message_id", describeField "message_id" "ID of the message to read" (emptySchema TInteger))

    ]

    ["message_id"]



instance FromJSON ReadMessageArgs where

  parseJSON = withObject "ReadMessageArgs" $ \v ->

    ReadMessageArgs <$> v .: "message_id"



-- | Graph definition for read_message.

data ReadMessageGraph mode = ReadMessageGraph

  { rmEntry :: mode :- EntryNode ReadMessageArgs

      :@ MCPExport

      :@ MCPToolDef ('("read_message", "Read full content of a message."))



  , rmRun :: mode :- LogicNode

      :@ Input ReadMessageArgs

      :@ UsesEffects '[GitHub, Goto Exit (Maybe MessageDetail)]



  , rmExit :: mode :- ExitNode (Maybe MessageDetail)

  }

  deriving Generic



-- | Logic for read_message.

readMessageLogic

  :: Member GitHub es

  => ReadMessageArgs

  -> Eff es (GotoChoice '[To Exit (Maybe MessageDetail)])

readMessageLogic args = do

  -- TODO: Configurable repo

  let repo = Repo "exomonad/exomonad"

  mIssue <- getIssue repo args.rmaMessageId False

  pure $ gotoExit $ fmap issueToDetail mIssue



issueToDetail :: Issue -> MessageDetail

issueToDetail i = MessageDetail

  { mdId = i.issueNumber

  , mdFrom = findLabelPrefix "from:" i.issueLabels

  , mdTo = findLabelPrefix "to:" i.issueLabels

  , mdMsgType = findLabelPrefix "msgtype:" i.issueLabels

  , mdSubject = i.issueTitle

  , mdBody = i.issueBody

  , mdStatus = T.pack $ show i.issueState

  }



-- ════════════════════════════════════════════════════════════════════════════

-- MARK READ

-- ════════════════════════════════════════════════════════════════════════════



data MarkReadArgs = MarkReadArgs

  { mraMessageId :: Int

  , mraReason    :: Maybe Text

  }

  deriving stock (Show, Eq, Generic)



instance HasJSONSchema MarkReadArgs where

  jsonSchema = objectSchema

    [ ("message_id", describeField "message_id" "ID of the message to mark as read" (emptySchema TInteger))

    , ("reason", describeField "reason" "Acknowledgment or action taken" (emptySchema TString))

    ]

    ["message_id"]



instance FromJSON MarkReadArgs where

  parseJSON = withObject "MarkReadArgs" $ \v ->

    MarkReadArgs

      <$> v .: "message_id"

      <*> v .:? "reason"



-- | Graph definition for mark_read.

data MarkReadGraph mode = MarkReadGraph

  { mrEntry :: mode :- EntryNode MarkReadArgs

      :@ MCPExport

      :@ MCPToolDef ('("mark_read", "Mark a message as read (closes the issue)."))



  , mrRun :: mode :- LogicNode

      :@ Input MarkReadArgs

      :@ UsesEffects '[GitHub, Goto Exit ()]



  , mrExit :: mode :- ExitNode ()

  }

  deriving Generic



-- | Logic for mark_read.

markReadLogic

  :: Member GitHub es

  => MarkReadArgs

  -> Eff es (GotoChoice '[To Exit ()])

markReadLogic args = do

  -- TODO: Configurable repo

  let repo = Repo "exomonad/exomonad"

  closeIssue repo args.mraMessageId

  pure $ gotoExit ()
