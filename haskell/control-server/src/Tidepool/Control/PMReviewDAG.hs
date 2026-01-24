{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Strategic planning tool for PMs to analyze the bead DAG.
module Tidepool.Control.PMReviewDAG
  ( PmReviewDagArgs(..)
  , PmReviewDagResult(..)
  , PmReviewDagGraph(..)
  , pmReviewDagLogic
  , pmReviewDagHandlers
  , BlockedInfo(..)
  , PriorityGap(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:?), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (diffUTCTime)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)

import Tidepool.Effects.BD (BD, listBeads, defaultListBeadsInput, BeadInfo(..), BeadStatus(..), DependencyInfo(..), DependencyType(..))
import Tidepool.Effect.Types (Time, getCurrentTime)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, LogicNode, ExitNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, describeField, emptySchema, SchemaType(..))

-- | Arguments for pm_review_dag tool.
data PmReviewDagArgs = PmReviewDagArgs
  { prdaIncludeClosed :: Maybe Bool
  , prdaAgingThresholdHours :: Maybe Int
  , prdaFocusTrack :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PmReviewDagArgs where
  jsonSchema = objectSchema
    [ ("include_closed", describeField "include_closed" "Include closed beads in the analysis" (emptySchema TBoolean))
    , ("aging_threshold_hours", describeField "aging_threshold_hours" "Threshold for aging analysis (default: 24h)" (emptySchema TInteger))
    , ("focus_track", describeField "focus_track" "Optional: focus on a specific track (label)" (emptySchema TString))
    ]
    []

instance FromJSON PmReviewDagArgs where
  parseJSON = withObject "PmReviewDagArgs" $ \v ->
    PmReviewDagArgs
      <$> v .:? "include_closed"
      <*> v .:? "aging_threshold_hours"
      <*> v .:? "focus_track"

instance ToJSON PmReviewDagArgs where
  toJSON args = object
    [ "include_closed" .= args.prdaIncludeClosed
    , "aging_threshold_hours" .= args.prdaAgingThresholdHours
    , "focus_track" .= args.prdaFocusTrack
    ]

-- | Result of pm_review_dag tool.
data PmReviewDagResult = PmReviewDagResult
  { prdrReady           :: [Text] -- ^ Beads in 'Ready' state (or open and unblocked)
  , prdrBlocked         :: [BlockedInfo] -- ^ Blocked beads with their max depth
  , prdrCriticalPath    :: [Text] -- ^ Longest chain to completion
  , prdrPriorityGaps    :: [PriorityGap] -- ^ (Blocked Bead, Blocker Bead) where blocker.priority > blocked.priority
  , prdrAging           :: [Text] -- ^ Beads older than threshold
  , prdrNeedsTlReview   :: [Text] -- ^ Beads with 'needs-tl-review' label
  , prdrNeedsPmApproval :: [Text] -- ^ Beads with 'needs-pm-approval' label
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Information about a blocked bead.
data BlockedInfo = BlockedInfo
  { biBeadId :: Text
  , biDepth  :: Int
  , biBlockers :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Information about a priority gap.
data PriorityGap = PriorityGap
  { pgBlockedId :: Text
  , pgBlockedPriority :: Int
  , pgBlockerId :: Text
  , pgBlockerPriority :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PriorityGap where
  toJSON pg = object
    [ "blocked_id" .= pg.pgBlockedId
    , "blocked_priority" .= pg.pgBlockedPriority
    , "blocker_id" .= pg.pgBlockerId
    , "blocker_priority" .= pg.pgBlockerPriority
    ]

-- | Graph definition for pm_review_dag tool.
data PmReviewDagGraph mode = PmReviewDagGraph
  { prdEntry :: mode :- EntryNode PmReviewDagArgs
      :@ MCPExport
      :@ MCPToolDef ('("pm_review_dag", "Analyze the bead DAG for strategic planning, identifying critical paths and priority inversions."))
      :@ MCPRoleHint 'PM

  , prdRun :: mode :- LogicNode
      :@ Input PmReviewDagArgs
      :@ UsesEffects '[BD, Time, Goto Exit PmReviewDagResult]

  , prdExit :: mode :- ExitNode PmReviewDagResult
  }
  deriving Generic

-- | Handlers for pm_review_dag graph.
pmReviewDagHandlers :: (Member BD effs, Member Time effs) => PmReviewDagGraph (AsHandler effs)
pmReviewDagHandlers = PmReviewDagGraph
  { prdEntry = ()
  , prdRun = pmReviewDagLogic
  , prdExit = ()
  }

-- | Logic for pm_review_dag tool.
pmReviewDagLogic
  :: (Member BD effs, Member Time effs)
  => PmReviewDagArgs
  -> Eff effs (GotoChoice '[To Exit PmReviewDagResult])
pmReviewDagLogic args = do
  -- 1. Fetch all beads
  allBeads <- listBeads defaultListBeadsInput
  now <- getCurrentTime
  
  -- 2. Extract relevant sets
  let includeClosed = fromMaybe False args.prdaIncludeClosed
      activeBeads = if includeClosed 
                    then allBeads 
                    else filter (\b -> b.biStatus /= StatusClosed) allBeads
      
      -- Filter by focus track if requested
      focusBeads = case args.prdaFocusTrack of
        Nothing -> activeBeads
        Just track -> filter (\b -> track `elem` b.biLabels) activeBeads
      
      allBeadMap = Map.fromList [ (b.biId, b) | b <- allBeads ]

  -- 3. Aging
  let agingThreshold = fromMaybe 24 args.prdaAgingThresholdHours
      isAging b = case b.biUpdatedAt of
        Nothing -> False
        Just updated -> 
          let diff = diffUTCTime now updated
          in diff > fromIntegral (agingThreshold * 3600)
      aging = [ b.biId | b <- focusBeads, isAging b ]

  -- 4. Workflow labels
  let needsTlReview = [ b.biId | b <- focusBeads, "needs-tl-review" `elem` b.biLabels ]
      needsPmApproval = [ b.biId | b <- focusBeads, "needs-pm-approval" `elem` b.biLabels ]
  
  -- 5. DAG Analysis helpers
  let getBlockers b = [ d.diId | d <- b.biDependencies, d.diDepType == DepDependsOn || d.diDepType == DepBlocks ]
      isDone bid = case Map.lookup bid allBeadMap of
        Just b -> b.biStatus == StatusClosed
        Nothing -> True -- Assume external deps are done
      activeBlockers b = filter (not . isDone) (getBlockers b)
      
  -- 6. Depth and Blocked Info
  -- depth(b) = 0 if not blocked
  -- depth(b) = 1 + max(depth of blockers)
  let 
    computeDepthMemo :: Text -> Set.Set Text -> Map.Map Text Int -> Map.Map Text Int
    computeDepthMemo bid visited memo
      | bid `Set.member` visited = Map.insert bid 0 memo -- Cycle detected
      | Map.member bid memo = memo
      | otherwise =
          case Map.lookup bid allBeadMap of
            Nothing -> Map.insert bid 0 memo
            Just b ->
              let blockers = activeBlockers b
              in if null blockers
                 then Map.insert bid 0 memo
                 else
                   let 
                     newVisited = Set.insert bid visited
                     memoWithBlockers = foldr (\blk m -> computeDepthMemo blk newVisited m) memo blockers
                     maxBlockerDepth = maximum (0 : [ fromMaybe 0 (Map.lookup blk memoWithBlockers) | blk <- blockers ])
                   in Map.insert bid (1 + maxBlockerDepth) memoWithBlockers

    allDepths = foldr (\b m -> computeDepthMemo b.biId Set.empty m) Map.empty allBeads

    blockedInfo = mapMaybe (\b -> 
        let blockers = activeBlockers b
            depth = fromMaybe 0 (Map.lookup b.biId allDepths)
        in if null blockers then Nothing else Just (BlockedInfo b.biId depth blockers)
      ) focusBeads
    
    ready = [ b.biId | b <- focusBeads, null (activeBlockers b) ]

  -- 7. Priority Gaps
  let priorityGaps = [ PriorityGap b.biId b.biPriority blockerId blocker.biPriority
                     | b <- focusBeads
                     , blockerId <- activeBlockers b
                     , Just blocker <- [Map.lookup blockerId allBeadMap]
                     , blocker.biPriority > b.biPriority 
                     ]

  -- 8. Critical Path
  -- The critical path is the chain leading to the bead with the maximum depth.
  let 
    maxDepthBeadId = if null focusBeads then Nothing 
                     else Just $ fst $ maximumBy (comparing snd) 
                          [ (b.biId, fromMaybe 0 (Map.lookup b.biId allDepths)) | b <- focusBeads ]
    
    getPath :: Text -> [Text]
    getPath bid = case Map.lookup bid allBeadMap of
      Nothing -> [bid]
      Just b ->
        let blockers = activeBlockers b
        in if null blockers
           then [bid]
           else 
             let deepestBlocker = maximumBy (comparing (\blk -> fromMaybe 0 (Map.lookup blk allDepths))) blockers
             in bid : getPath deepestBlocker

    criticalPath = case maxDepthBeadId of
      Nothing -> []
      Just bid -> getPath bid

  pure $ gotoExit PmReviewDagResult
    { prdrReady = ready
    , prdrBlocked = sortOn (negate . biDepth) blockedInfo
    , prdrCriticalPath = criticalPath
    , prdrPriorityGaps = priorityGaps
    , prdrAging = aging
    , prdrNeedsTlReview = needsTlReview
    , prdrNeedsPmApproval = needsPmApproval
    }