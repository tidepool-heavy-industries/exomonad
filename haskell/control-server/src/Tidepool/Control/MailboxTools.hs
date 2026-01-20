{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tidepool.Control.MailboxTools
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

import Tidepool.Effects.BD
  ( BD
  , CreateBeadInput(..)
  , defaultCreateInput
  , ListBeadsInput(..)
  , defaultListBeadsInput
  , BeadInfo(..)
  , BeadType(..)
  , BeadStatus(..)
  , DependencyType(..)
  , createBead
  , listBeads
  , getBead
  , closeBead
  , addDep
  )
import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, describeField, emptySchema, SchemaType(..))

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
  , blockWork :: Maybe Text -- BeadId
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SendRequest where
  jsonSchema = objectSchema
    [ ("to", describeField "to" "Recipient role (e.g., 'pm', 'tl')" (emptySchema TString))
    , ("msgType", describeField "msgType" "Message type (proposal, question, briefing, approval)" (emptySchema TString))
    , ("subject", describeField "subject" "Message subject" (emptySchema TString))
    , ("body", describeField "body" "Markdown message content" (emptySchema TString))
    , ("blockWork", describeField "blockWork" "Optional: make a work bead depend on this message" (emptySchema TString))
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
      :@ MCPToolDef ('("send_message", "Send a message to another agent using beads."))

  , smRun :: mode :- LogicNode
      :@ Input SendRequest
      :@ UsesEffects '[BD, Goto Exit Text]

  , smExit :: mode :- ExitNode Text
  }
  deriving Generic

-- | Logic for send_message.
sendMessageLogic
  :: Member BD es
  => Role
  -> SendRequest
  -> Eff es (GotoChoice '[To Exit Text])
sendMessageLogic fromRole req = do
  let labels = ["mailbox", "from:" <> fromRole, "to:" <> req.to, "msgtype:" <> req.msgType]
  msgId <- createBead defaultCreateInput
    { cbiTitle = req.subject
    , cbiDescription = Just req.body
    , cbiType = TypeMessage
    , cbiLabels = labels
    , cbiAssignee = Just req.to
    }
  
  case req.blockWork of
    Just workId -> addDep workId msgId DepDependsOn
    Nothing -> pure ()
    
  pure $ gotoExit msgId

-- ════════════════════════════════════════════════════════════════════════════
-- CHECK INBOX
-- ════════════════════════════════════════════════════════════════════════════

-- | Summary of a message in the inbox.
data MessageSummary = MessageSummary
  { msId      :: Text
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
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema CheckInboxArgs where
  jsonSchema = objectSchema [] []

-- | Graph definition for check_inbox.
data CheckInboxGraph mode = CheckInboxGraph
  { ciEntry :: mode :- EntryNode CheckInboxArgs
      :@ MCPExport
      :@ MCPToolDef ('("check_inbox", "Check current agent's message inbox."))

  , ciRun :: mode :- LogicNode
      :@ Input CheckInboxArgs
      :@ UsesEffects '[BD, Goto Exit [MessageSummary]]

  , ciExit :: mode :- ExitNode [MessageSummary]
  }
  deriving Generic

-- | Logic for check_inbox.
checkInboxLogic
  :: Member BD es
  => Role
  -> CheckInboxArgs
  -> Eff es (GotoChoice '[To Exit [MessageSummary]])
checkInboxLogic myRole _ = do
  beads <- listBeads defaultListBeadsInput
    { lbiStatus = Just StatusOpen
    , lbiLabels = ["mailbox", "to:" <> myRole]
    }
  pure $ gotoExit $ map beadToSummary beads

beadToSummary :: BeadInfo -> MessageSummary
beadToSummary b = MessageSummary
  { msId = b.biId
  , msFrom = findLabelPrefix "from:" b.biLabels
  , msMsgType = findLabelPrefix "msgtype:" b.biLabels
  , msSubject = b.biTitle
  , msStatus = T.pack $ show b.biStatus
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
  { mdId      :: Text
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
  { rmaMessageId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ReadMessageArgs where
  jsonSchema = objectSchema
    [ ("message_id", describeField "message_id" "ID of the message to read" (emptySchema TString))
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
      :@ UsesEffects '[BD, Goto Exit (Maybe MessageDetail)]

  , rmExit :: mode :- ExitNode (Maybe MessageDetail)
  }
  deriving Generic

-- | Logic for read_message.
readMessageLogic
  :: Member BD es
  => ReadMessageArgs
  -> Eff es (GotoChoice '[To Exit (Maybe MessageDetail)])
readMessageLogic args = do
  mBead <- getBead args.rmaMessageId
  pure $ gotoExit $ fmap beadToDetail mBead

beadToDetail :: BeadInfo -> MessageDetail
beadToDetail b = MessageDetail
  { mdId = b.biId
  , mdFrom = findLabelPrefix "from:" b.biLabels
  , mdTo = findLabelPrefix "to:" b.biLabels
  , mdMsgType = findLabelPrefix "msgtype:" b.biLabels
  , mdSubject = b.biTitle
  , mdBody = fromMaybe "" b.biDescription
  , mdStatus = T.pack $ show b.biStatus
  }

-- ════════════════════════════════════════════════════════════════════════════
-- MARK READ
-- ════════════════════════════════════════════════════════════════════════════

data MarkReadArgs = MarkReadArgs
  { mraMessageId :: Text
  , mraReason    :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema MarkReadArgs where
  jsonSchema = objectSchema
    [ ("message_id", describeField "message_id" "ID of the message to mark as read" (emptySchema TString))
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
      :@ MCPToolDef ('("mark_read", "Mark a message as read (closes the bead)."))

  , mrRun :: mode :- LogicNode
      :@ Input MarkReadArgs
      :@ UsesEffects '[BD, Goto Exit ()]

  , mrExit :: mode :- ExitNode ()
  }
  deriving Generic

-- | Logic for mark_read.
markReadLogic
  :: Member BD es
  => MarkReadArgs
  -> Eff es (GotoChoice '[To Exit ()])
markReadLogic args = do
  closeBead args.mraMessageId args.mraReason
  pure $ gotoExit ()