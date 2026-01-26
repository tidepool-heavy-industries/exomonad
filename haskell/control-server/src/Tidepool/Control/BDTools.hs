{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | BD (Beads) MCP tools for centralized bead operations.
--
-- Exposes beads operations via MCP so agents can interact with the
-- task tracking system without needing local bd CLI access.
--
-- = Tools Exposed
--
-- == Read Operations
-- - bd_list: List beads with optional filters
-- - bd_show: Get detailed bead info by ID
-- - bd_ready: List beads ready to work (no blockers)
--
-- == Write Operations
-- - bd_create: Create a new bead
-- - bd_update: Update bead fields
-- - bd_close: Close a bead
-- - bd_add_dep: Add dependency between beads
-- - bd_add_label: Add label to a bead
-- - bd_sync: Sync beads with remote
module Tidepool.Control.BDTools
  ( -- * List Tool
    BDListGraph(..)
  , bdListLogic
  , BDListArgs(..)
  , BDListResult(..)

    -- * Show Tool
  , BDShowGraph(..)
  , bdShowLogic
  , BDShowArgs(..)
  , BDShowResult(..)

    -- * Ready Tool
  , BDReadyGraph(..)
  , bdReadyLogic
  , BDReadyArgs(..)
  , BDReadyResult(..)

    -- * Create Tool
  , BDCreateGraph(..)
  , bdCreateLogic
  , BDCreateArgs(..)
  , BDCreateResult(..)

    -- * Update Tool
  , BDUpdateGraph(..)
  , bdUpdateLogic
  , BDUpdateArgs(..)
  , BDUpdateResult(..)

    -- * Close Tool
  , BDCloseGraph(..)
  , bdCloseLogic
  , BDCloseArgs(..)
  , BDCloseResult(..)

    -- * Add Dependency Tool
  , BDAddDepGraph(..)
  , bdAddDepLogic
  , BDAddDepArgs(..)
  , BDAddDepResult(..)

    -- * Add Label Tool
  , BDAddLabelGraph(..)
  , bdAddLabelLogic
  , BDAddLabelArgs(..)
  , BDAddLabelResult(..)

    -- * Sync Tool
  , BDSyncGraph(..)
  , bdSyncLogic
  , BDSyncArgs(..)
  , BDSyncResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:), (.:?), (.=)
  , object, withObject, withText
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD
  ( BD, BeadInfo(..), BeadStatus(..), BeadType(..), DependencyType(..)
  , CreateBeadInput(..), UpdateBeadInput(..), ListBeadsInput(..)
  , defaultCreateInput, emptyUpdateInput, defaultListBeadsInput
  , getBead, listBeads, createBead, updateBead, closeBead
  , addDep, addLabel, sync
  )
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema
  ( HasJSONSchema(..), objectSchema, arraySchema, emptySchema
  , SchemaType(..), describeField
  )


-- ════════════════════════════════════════════════════════════════════════════
-- BD LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_list tool.
data BDListArgs = BDListArgs
  { blaStatus :: Maybe Text    -- ^ Filter by status: open, in_progress, closed, blocked
  , blaType :: Maybe Text      -- ^ Filter by type: task, bug, feature, epic
  , blaLabels :: Maybe [Text]  -- ^ Filter by labels (all must match)
  , blaAssignee :: Maybe Text  -- ^ Filter by assignee
  , blaParent :: Maybe Text    -- ^ Filter by parent bead ID
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDListArgs where
  jsonSchema = objectSchema
    [ ("status", describeField "status" "Filter by status (open, in_progress, closed, blocked)" (emptySchema TString))
    , ("type", describeField "type" "Filter by type (task, bug, feature, epic)" (emptySchema TString))
    , ("labels", describeField "labels" "Filter by labels (all must match)" (arraySchema (emptySchema TString)))
    , ("assignee", describeField "assignee" "Filter by assignee" (emptySchema TString))
    , ("parent", describeField "parent" "Filter by parent bead ID" (emptySchema TString))
    ]
    []  -- No required fields

instance FromJSON BDListArgs where
  parseJSON = withObject "BDListArgs" $ \v ->
    BDListArgs
      <$> v .:? "status"
      <*> v .:? "type"
      <*> v .:? "labels"
      <*> v .:? "assignee"
      <*> v .:? "parent"

instance ToJSON BDListArgs where
  toJSON args = object
    [ "status" .= blaStatus args
    , "type" .= blaType args
    , "labels" .= blaLabels args
    , "assignee" .= blaAssignee args
    , "parent" .= blaParent args
    ]

-- | Result of bd_list tool.
data BDListResult = BDListResult
  { blrBeads :: [BeadInfo]
  , blrCount :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDListResult where
  toJSON res = object
    [ "beads" .= blrBeads res
    , "count" .= blrCount res
    ]

-- | Graph definition for bd_list tool.
data BDListGraph mode = BDListGraph
  { blEntry :: mode :- EntryNode BDListArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_list", "List beads with optional status/type/label/assignee filters.")

  , blRun :: mode :- LogicNode
      :@ Input BDListArgs
      :@ UsesEffects '[BD, Goto Exit BDListResult]

  , blExit :: mode :- ExitNode BDListResult
  }
  deriving Generic

-- | Core logic for bd_list.
bdListLogic
  :: (Member BD es)
  => BDListArgs
  -> Eff es (GotoChoice '[To Exit BDListResult])
bdListLogic args = do
  let input = defaultListBeadsInput
        { lbiStatus = parseStatus =<< args.blaStatus
        , lbiType = parseType =<< args.blaType
        , lbiLabels = fromMaybe [] args.blaLabels
        , lbiAssignee = args.blaAssignee
        , lbiParent = args.blaParent
        }
  beads <- listBeads input
  pure $ gotoExit $ BDListResult
    { blrBeads = beads
    , blrCount = length beads
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_show tool.
data BDShowArgs = BDShowArgs
  { bsaBeadId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDShowArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "The bead ID to show" (emptySchema TString))
    ]
    ["bead_id"]

instance FromJSON BDShowArgs where
  parseJSON = withObject "BDShowArgs" $ \v ->
    BDShowArgs <$> v .: "bead_id"

instance ToJSON BDShowArgs where
  toJSON args = object ["bead_id" .= bsaBeadId args]

-- | Result of bd_show tool.
data BDShowResult = BDShowResult
  { bsrBead :: Maybe BeadInfo
  , bsrFound :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDShowResult where
  toJSON res = object
    [ "bead" .= bsrBead res
    , "found" .= bsrFound res
    ]

-- | Graph definition for bd_show tool.
data BDShowGraph mode = BDShowGraph
  { bsEntry :: mode :- EntryNode BDShowArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_show", "Get detailed information about a specific bead by ID.")

  , bsRun :: mode :- LogicNode
      :@ Input BDShowArgs
      :@ UsesEffects '[BD, Goto Exit BDShowResult]

  , bsExit :: mode :- ExitNode BDShowResult
  }
  deriving Generic

-- | Core logic for bd_show.
bdShowLogic
  :: (Member BD es)
  => BDShowArgs
  -> Eff es (GotoChoice '[To Exit BDShowResult])
bdShowLogic args = do
  maybeBead <- getBead args.bsaBeadId
  pure $ gotoExit $ BDShowResult
    { bsrBead = maybeBead
    , bsrFound = case maybeBead of
        Just _ -> True
        Nothing -> False
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD READY TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_ready tool.
data BDReadyArgs = BDReadyArgs
  { braAssignee :: Maybe Text  -- ^ Optional: filter by assignee
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDReadyArgs where
  jsonSchema = objectSchema
    [ ("assignee", describeField "assignee" "Optional: filter ready beads by assignee" (emptySchema TString))
    ]
    []

instance FromJSON BDReadyArgs where
  parseJSON = withObject "BDReadyArgs" $ \v ->
    BDReadyArgs <$> v .:? "assignee"

instance ToJSON BDReadyArgs where
  toJSON args = object ["assignee" .= braAssignee args]

-- | Result of bd_ready tool.
data BDReadyResult = BDReadyResult
  { brrBeads :: [BeadInfo]
  , brrCount :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDReadyResult where
  toJSON res = object
    [ "beads" .= brrBeads res
    , "count" .= brrCount res
    ]

-- | Graph definition for bd_ready tool.
data BDReadyGraph mode = BDReadyGraph
  { brEntry :: mode :- EntryNode BDReadyArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_ready", "List beads that are ready to work on (open status, no blockers).")

  , brRun :: mode :- LogicNode
      :@ Input BDReadyArgs
      :@ UsesEffects '[BD, Goto Exit BDReadyResult]

  , brExit :: mode :- ExitNode BDReadyResult
  }
  deriving Generic

-- | Core logic for bd_ready.
bdReadyLogic
  :: (Member BD es)
  => BDReadyArgs
  -> Eff es (GotoChoice '[To Exit BDReadyResult])
bdReadyLogic args = do
  -- Get open beads, then filter those with no blockers
  let input = defaultListBeadsInput
        { lbiStatus = Just StatusOpen
        , lbiAssignee = args.braAssignee
        }
  allOpen <- listBeads input
  -- Filter to only those with no dependencies (ready to work)
  let ready = filter hasNoBlockers allOpen
  pure $ gotoExit $ BDReadyResult
    { brrBeads = ready
    , brrCount = length ready
    }
  where
    hasNoBlockers bead = null bead.biDependencies


-- ════════════════════════════════════════════════════════════════════════════
-- BD CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_create tool.
data BDCreateArgs = BDCreateArgs
  { bcaTitle :: Text
  , bcaDescription :: Maybe Text
  , bcaType :: Maybe Text       -- ^ task, bug, feature, epic
  , bcaPriority :: Maybe Int    -- ^ 0-4
  , bcaLabels :: Maybe [Text]
  , bcaAssignee :: Maybe Text
  , bcaParent :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDCreateArgs where
  jsonSchema = objectSchema
    [ ("title", describeField "title" "Title of the new bead" (emptySchema TString))
    , ("description", describeField "description" "Description of the bead" (emptySchema TString))
    , ("type", describeField "type" "Bead type: task, bug, feature, epic (default: task)" (emptySchema TString))
    , ("priority", describeField "priority" "Priority 0-4 (0=critical, 4=backlog, default: 2)" (emptySchema TNumber))
    , ("labels", describeField "labels" "Labels to attach" (arraySchema (emptySchema TString)))
    , ("assignee", describeField "assignee" "Assignee username" (emptySchema TString))
    , ("parent", describeField "parent" "Parent bead ID for hierarchy" (emptySchema TString))
    ]
    ["title"]

instance FromJSON BDCreateArgs where
  parseJSON = withObject "BDCreateArgs" $ \v ->
    BDCreateArgs
      <$> v .: "title"
      <*> v .:? "description"
      <*> v .:? "type"
      <*> v .:? "priority"
      <*> v .:? "labels"
      <*> v .:? "assignee"
      <*> v .:? "parent"

instance ToJSON BDCreateArgs where
  toJSON args = object
    [ "title" .= bcaTitle args
    , "description" .= bcaDescription args
    , "type" .= bcaType args
    , "priority" .= bcaPriority args
    , "labels" .= bcaLabels args
    , "assignee" .= bcaAssignee args
    , "parent" .= bcaParent args
    ]

-- | Result of bd_create tool.
data BDCreateResult = BDCreateResult
  { bcrBeadId :: Text
  , bcrSuccess :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDCreateResult where
  toJSON res = object
    [ "bead_id" .= bcrBeadId res
    , "success" .= bcrSuccess res
    ]

-- | Graph definition for bd_create tool.
data BDCreateGraph mode = BDCreateGraph
  { bcEntry :: mode :- EntryNode BDCreateArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_create", "Create a new bead with the specified title, type, priority, and labels.")

  , bcRun :: mode :- LogicNode
      :@ Input BDCreateArgs
      :@ UsesEffects '[BD, Goto Exit BDCreateResult]

  , bcExit :: mode :- ExitNode BDCreateResult
  }
  deriving Generic

-- | Core logic for bd_create.
bdCreateLogic
  :: (Member BD es)
  => BDCreateArgs
  -> Eff es (GotoChoice '[To Exit BDCreateResult])
bdCreateLogic args = do
  let input = defaultCreateInput
        { cbiTitle = args.bcaTitle
        , cbiDescription = args.bcaDescription
        , cbiType = fromMaybe TypeTask (parseType =<< args.bcaType)
        , cbiPriority = fromMaybe 2 args.bcaPriority
        , cbiLabels = fromMaybe [] args.bcaLabels
        , cbiAssignee = args.bcaAssignee
        , cbiParent = args.bcaParent
        }
  beadId <- createBead input
  pure $ gotoExit $ BDCreateResult
    { bcrBeadId = beadId
    , bcrSuccess = True
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_update tool.
data BDUpdateArgs = BDUpdateArgs
  { buaBeadId :: Text
  , buaTitle :: Maybe Text
  , buaDescription :: Maybe Text
  , buaStatus :: Maybe Text     -- ^ open, in_progress, closed, blocked
  , buaPriority :: Maybe Int
  , buaAssignee :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDUpdateArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "The bead ID to update" (emptySchema TString))
    , ("title", describeField "title" "New title" (emptySchema TString))
    , ("description", describeField "description" "New description" (emptySchema TString))
    , ("status", describeField "status" "New status: open, in_progress, closed, blocked" (emptySchema TString))
    , ("priority", describeField "priority" "New priority 0-4" (emptySchema TNumber))
    , ("assignee", describeField "assignee" "New assignee" (emptySchema TString))
    ]
    ["bead_id"]

instance FromJSON BDUpdateArgs where
  parseJSON = withObject "BDUpdateArgs" $ \v ->
    BDUpdateArgs
      <$> v .: "bead_id"
      <*> v .:? "title"
      <*> v .:? "description"
      <*> v .:? "status"
      <*> v .:? "priority"
      <*> v .:? "assignee"

instance ToJSON BDUpdateArgs where
  toJSON args = object
    [ "bead_id" .= buaBeadId args
    , "title" .= buaTitle args
    , "description" .= buaDescription args
    , "status" .= buaStatus args
    , "priority" .= buaPriority args
    , "assignee" .= buaAssignee args
    ]

-- | Result of bd_update tool.
data BDUpdateResult = BDUpdateResult
  { burSuccess :: Bool
  , burBeadId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDUpdateResult where
  toJSON res = object
    [ "success" .= burSuccess res
    , "bead_id" .= burBeadId res
    ]

-- | Graph definition for bd_update tool.
data BDUpdateGraph mode = BDUpdateGraph
  { buEntry :: mode :- EntryNode BDUpdateArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_update", "Update bead fields (title, description, status, priority, assignee).")

  , buRun :: mode :- LogicNode
      :@ Input BDUpdateArgs
      :@ UsesEffects '[BD, Goto Exit BDUpdateResult]

  , buExit :: mode :- ExitNode BDUpdateResult
  }
  deriving Generic

-- | Core logic for bd_update.
bdUpdateLogic
  :: (Member BD es)
  => BDUpdateArgs
  -> Eff es (GotoChoice '[To Exit BDUpdateResult])
bdUpdateLogic args = do
  let input = emptyUpdateInput
        { ubiTitle = args.buaTitle
        , ubiDescription = args.buaDescription
        , ubiStatus = parseStatus =<< args.buaStatus
        , ubiPriority = args.buaPriority
        , ubiAssignee = args.buaAssignee
        }
  updateBead args.buaBeadId input
  pure $ gotoExit $ BDUpdateResult
    { burSuccess = True
    , burBeadId = args.buaBeadId
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_close tool.
data BDCloseArgs = BDCloseArgs
  { bclBeadId :: Text
  , bclReason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDCloseArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "The bead ID to close" (emptySchema TString))
    , ("reason", describeField "reason" "Optional reason for closing" (emptySchema TString))
    ]
    ["bead_id"]

instance FromJSON BDCloseArgs where
  parseJSON = withObject "BDCloseArgs" $ \v ->
    BDCloseArgs
      <$> v .: "bead_id"
      <*> v .:? "reason"

instance ToJSON BDCloseArgs where
  toJSON args = object
    [ "bead_id" .= bclBeadId args
    , "reason" .= bclReason args
    ]

-- | Result of bd_close tool.
data BDCloseResult = BDCloseResult
  { bclrSuccess :: Bool
  , bclrBeadId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDCloseResult where
  toJSON res = object
    [ "success" .= bclrSuccess res
    , "bead_id" .= bclrBeadId res
    ]

-- | Graph definition for bd_close tool.
data BDCloseGraph mode = BDCloseGraph
  { bclEntry :: mode :- EntryNode BDCloseArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_close", "Close a bead with optional reason.")

  , bclRun :: mode :- LogicNode
      :@ Input BDCloseArgs
      :@ UsesEffects '[BD, Goto Exit BDCloseResult]

  , bclExit :: mode :- ExitNode BDCloseResult
  }
  deriving Generic

-- | Core logic for bd_close.
bdCloseLogic
  :: (Member BD es)
  => BDCloseArgs
  -> Eff es (GotoChoice '[To Exit BDCloseResult])
bdCloseLogic args = do
  closeBead args.bclBeadId args.bclReason
  pure $ gotoExit $ BDCloseResult
    { bclrSuccess = True
    , bclrBeadId = args.bclBeadId
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD ADD DEPENDENCY TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_add_dep tool.
data BDAddDepArgs = BDAddDepArgs
  { badFromId :: Text       -- ^ The bead that depends on another
  , badToId :: Text         -- ^ The bead being depended on
  , badDepType :: Maybe Text  -- ^ depends-on (default), blocks, parent-child
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDAddDepArgs where
  jsonSchema = objectSchema
    [ ("from_id", describeField "from_id" "The bead ID that depends on another" (emptySchema TString))
    , ("to_id", describeField "to_id" "The bead ID being depended on" (emptySchema TString))
    , ("dep_type", describeField "dep_type" "Dependency type: depends-on (default), blocks, parent-child" (emptySchema TString))
    ]
    ["from_id", "to_id"]

instance FromJSON BDAddDepArgs where
  parseJSON = withObject "BDAddDepArgs" $ \v ->
    BDAddDepArgs
      <$> v .: "from_id"
      <*> v .: "to_id"
      <*> v .:? "dep_type"

instance ToJSON BDAddDepArgs where
  toJSON args = object
    [ "from_id" .= badFromId args
    , "to_id" .= badToId args
    , "dep_type" .= badDepType args
    ]

-- | Result of bd_add_dep tool.
data BDAddDepResult = BDAddDepResult
  { badrSuccess :: Bool
  , badrFromId :: Text
  , badrToId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDAddDepResult where
  toJSON res = object
    [ "success" .= badrSuccess res
    , "from_id" .= badrFromId res
    , "to_id" .= badrToId res
    ]

-- | Graph definition for bd_add_dep tool.
data BDAddDepGraph mode = BDAddDepGraph
  { badEntry :: mode :- EntryNode BDAddDepArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_add_dep", "Add a dependency between two beads (from_id depends on to_id).")

  , badRun :: mode :- LogicNode
      :@ Input BDAddDepArgs
      :@ UsesEffects '[BD, Goto Exit BDAddDepResult]

  , badExit :: mode :- ExitNode BDAddDepResult
  }
  deriving Generic

-- | Core logic for bd_add_dep.
bdAddDepLogic
  :: (Member BD es)
  => BDAddDepArgs
  -> Eff es (GotoChoice '[To Exit BDAddDepResult])
bdAddDepLogic args = do
  let depType = fromMaybe DepDependsOn (parseDepType =<< args.badDepType)
  addDep args.badFromId args.badToId depType
  pure $ gotoExit $ BDAddDepResult
    { badrSuccess = True
    , badrFromId = args.badFromId
    , badrToId = args.badToId
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD ADD LABEL TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_add_label tool.
data BDAddLabelArgs = BDAddLabelArgs
  { balBeadId :: Text
  , balLabel :: Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDAddLabelArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "The bead ID to add label to" (emptySchema TString))
    , ("label", describeField "label" "The label to add" (emptySchema TString))
    ]
    ["bead_id", "label"]

instance FromJSON BDAddLabelArgs where
  parseJSON = withObject "BDAddLabelArgs" $ \v ->
    BDAddLabelArgs
      <$> v .: "bead_id"
      <*> v .: "label"

instance ToJSON BDAddLabelArgs where
  toJSON args = object
    [ "bead_id" .= balBeadId args
    , "label" .= balLabel args
    ]

-- | Result of bd_add_label tool.
data BDAddLabelResult = BDAddLabelResult
  { balrSuccess :: Bool
  , balrBeadId :: Text
  , balrLabel :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDAddLabelResult where
  toJSON res = object
    [ "success" .= balrSuccess res
    , "bead_id" .= balrBeadId res
    , "label" .= balrLabel res
    ]

-- | Graph definition for bd_add_label tool.
data BDAddLabelGraph mode = BDAddLabelGraph
  { balEntry :: mode :- EntryNode BDAddLabelArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_add_label", "Add a label to a bead.")

  , balRun :: mode :- LogicNode
      :@ Input BDAddLabelArgs
      :@ UsesEffects '[BD, Goto Exit BDAddLabelResult]

  , balExit :: mode :- ExitNode BDAddLabelResult
  }
  deriving Generic

-- | Core logic for bd_add_label.
bdAddLabelLogic
  :: (Member BD es)
  => BDAddLabelArgs
  -> Eff es (GotoChoice '[To Exit BDAddLabelResult])
bdAddLabelLogic args = do
  addLabel args.balBeadId args.balLabel
  pure $ gotoExit $ BDAddLabelResult
    { balrSuccess = True
    , balrBeadId = args.balBeadId
    , balrLabel = args.balLabel
    }


-- ════════════════════════════════════════════════════════════════════════════
-- BD SYNC TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for bd_sync tool.
data BDSyncArgs = BDSyncArgs
  { bsyDummy :: Maybe Text  -- Placeholder, sync takes no args
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema BDSyncArgs where
  jsonSchema = objectSchema [] []

instance FromJSON BDSyncArgs where
  parseJSON = withObject "BDSyncArgs" $ \_ ->
    pure $ BDSyncArgs Nothing

instance ToJSON BDSyncArgs where
  toJSON _ = object []

-- | Result of bd_sync tool.
data BDSyncResult = BDSyncResult
  { bsyrSuccess :: Bool
  , bsyrMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BDSyncResult where
  toJSON res = object
    [ "success" .= bsyrSuccess res
    , "message" .= bsyrMessage res
    ]

-- | Graph definition for bd_sync tool.
data BDSyncGraph mode = BDSyncGraph
  { bsyEntry :: mode :- EntryNode BDSyncArgs
      :@ MCPExport
      :@ MCPToolDef '("bd_sync", "Synchronize beads with the remote/main branch.")

  , bsyRun :: mode :- LogicNode
      :@ Input BDSyncArgs
      :@ UsesEffects '[BD, Goto Exit BDSyncResult]

  , bsyExit :: mode :- ExitNode BDSyncResult
  }
  deriving Generic

-- | Core logic for bd_sync.
bdSyncLogic
  :: (Member BD es)
  => BDSyncArgs
  -> Eff es (GotoChoice '[To Exit BDSyncResult])
bdSyncLogic _ = do
  sync
  pure $ gotoExit $ BDSyncResult
    { bsyrSuccess = True
    , bsyrMessage = "Beads synchronized with remote"
    }


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse status text to BeadStatus.
parseStatus :: Text -> Maybe BeadStatus
parseStatus t = case T.toLower t of
  "open"        -> Just StatusOpen
  "in_progress" -> Just StatusInProgress
  "closed"      -> Just StatusClosed
  "blocked"     -> Just StatusBlocked
  "hooked"      -> Just StatusHooked
  _             -> Nothing

-- | Parse type text to BeadType.
parseType :: Text -> Maybe BeadType
parseType t = case T.toLower t of
  "task"          -> Just TypeTask
  "bug"           -> Just TypeBug
  "feature"       -> Just TypeFeature
  "epic"          -> Just TypeEpic
  "merge-request" -> Just TypeMergeRequest
  "message"       -> Just TypeMessage
  _               -> Nothing

-- | Parse dependency type text to DependencyType.
parseDepType :: Text -> Maybe DependencyType
parseDepType t = case T.toLower t of
  "depends-on"   -> Just DepDependsOn
  "blocks"       -> Just DepBlocks
  "parent-child" -> Just DepParentChild
  _              -> Nothing
