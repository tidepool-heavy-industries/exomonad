{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render RunTree to human-readable and JSON formats.
--
-- = Human-Readable Output
--
-- @
-- ══════════════════════════════════════════════════════════════
--   RUN: 2026-01-12T17:39:42Z
--   SPEC: URL shortener with Storage and Api
-- ══════════════════════════════════════════════════════════════
--
-- work-758183 [URL shortener with Storage and Api]
--   │ SPAWN 3 children
--   │ ├─ spawn child-In-82fe25: Implement Storage module
--   │ ├─ spawn child-Im-ecb203: Implement Api module
--   │ ├─ spawn child-Im-f48963: Implement Storage module
--   │ AWAIT
--   │ ├─ done child-In-82fe25: ✓ e8cf578
--   │ AWAIT
--   │ ├─ done child-Im-ecb203: ✓ 4a94e42
--   │ AWAIT
--   │ ├─ done child-Im-f48963: ✓ 3de0330
--   │ COMPLETE 04bf88e
--   └─ ✓ 04bf88e
--
--   ├─► child-In-82fe25 [Implement Storage module]
--   │     │ CONTINUE
--   │     │ COMPLETE e8cf578
--   │     └─ ✓ e8cf578
--
-- ⚠️  ISSUES:
--   • Duplicate directive: "Implement Storage module" (2 children)
--
-- STATS: 13m38s │ 3/3 children succeeded │ 04bf88e
-- ══════════════════════════════════════════════════════════════
-- @
module TypesFirstDev.RunTree.Render
  ( -- * Rendering
    renderRunTree
  , renderNode
  , renderIssues

    -- * Output
  , writeRunTree
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale, NominalDiffTime)
import System.FilePath ((</>))
import Text.Printf (printf)

import TypesFirstDev.RunTree
import TypesFirstDev.Types.Core (Spec(..))


-- ════════════════════════════════════════════════════════════════════════════
-- RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a RunTree to human-readable text.
renderRunTree :: RunTree -> Text
renderRunTree tree = T.unlines
  [ "══════════════════════════════════════════════════════════════"
  , "  RUN: " <> formatTimeText tree.rtStartTime
  , "  SPEC: " <> tree.rtSpec.sDescription
  , "══════════════════════════════════════════════════════════════"
  , ""
  , renderNode "" tree.rtRoot
  , ""
  , renderIssues tree
  , ""
  , renderStats tree
  , "══════════════════════════════════════════════════════════════"
  ]


-- | Render a single node with its events and children.
renderNode :: Text -> Node -> Text
renderNode prefix node = T.unlines $
  -- Node header with session op indicator
  [ prefix <> branchName <> " [" <> T.take 60 node.nDirective <> "]" <> sessionOpTag ]
  -- Events with time deltas
  <> renderEventsWithDeltas (prefix <> "  │ ") node.nEvents
  -- Outcome
  <> [ prefix <> "  └─ " <> renderOutcome node.nOutcome ]
  -- Children (indented)
  <> concatMap (renderChildNode (prefix <> "  ")) node.nChildren
  where
    branchName = node.nSessionInfo.siBranch
    sessionOpTag = case node.nSessionOp of
      Just (OpFresh slug) -> " (fresh: " <> slug <> ")"
      Just OpContinue -> " (continue)"
      Just (OpFork slug) -> " (fork: " <> slug <> ")"
      Nothing -> ""


-- | Render a child node with connector prefix.
renderChildNode :: Text -> Node -> [Text]
renderChildNode prefix node =
  [ ""
  , prefix <> "├─► " <> node.nSessionInfo.siBranch <> " [" <> T.take 50 node.nDirective <> "]"
  ]
  <> renderEventsWithDeltas (prefix <> "│     │ ") node.nEvents
  <> [ prefix <> "│     └─ " <> renderOutcome node.nOutcome ]


-- | Render events with time deltas between them.
renderEventsWithDeltas :: Text -> [Timed Event] -> [Text]
renderEventsWithDeltas prefix events = case events of
  [] -> []
  (first:rest) -> renderEventWithDelta prefix Nothing first
                : zipWith (renderEventWithDelta prefix . Just) (map (.tTime) events) rest


-- | Render a single event with optional time delta from previous.
renderEventWithDelta :: Text -> Maybe UTCTime -> Timed Event -> Text
renderEventWithDelta prefix mPrevTime (Timed t ev) =
  let deltaText = case mPrevTime of
        Nothing -> ""
        Just prev ->
          let delta = diffUTCTime t prev
          in if delta > 1 then " +" <> formatDeltaShort delta else ""
  in prefix <> renderEventBody ev <> deltaText


-- | Render event body without prefix.
renderEventBody :: Event -> Text
renderEventBody ev = case ev of
  EvDecision (Spawn cs) ->
    "SPAWN " <> T.pack (show (length cs)) <> " children"
  EvDecision Continue ->
    "CONTINUE"
  EvDecision AwaitNext ->
    "AWAIT"
  EvDecision (Complete h) ->
    "COMPLETE " <> h
  EvDecision (PlanRevisionNeeded issue discovery proposed) ->
    "PLAN_REVISION_NEEDED:\n" <>
    "  Issue: " <> issue <> "\n" <>
    "  Discovery: " <> discovery <> "\n" <>
    "  Proposed: " <> proposed
  EvChildSpawned cid cs ->
    "├─ spawn " <> showChildId cid <> ": " <> cs.csDirective
  EvChildComplete _cid directive outcome ->
    "├─ done [" <> directive <> "]: " <> showOutcomeShort outcome
  EvCommit c ->
    "COMMIT " <> c.cHash <> " " <> c.cMessage
  EvMetrics cost turns ->
    "  ($" <> T.pack (printf "%.3f" cost) <> ", " <> T.pack (show turns) <> " turns)"


-- | Render an outcome.
renderOutcome :: Maybe ChildOutcome -> Text
renderOutcome Nothing = "⏳ running"
renderOutcome (Just (ChildSuccess hash)) = "✓ " <> hash
renderOutcome (Just (ChildFailure msg details)) =
  "✗ " <> msg <> maybe "" (\d -> "\n  Details: " <> d) details
renderOutcome (Just (ChildPlanRevision issue discovery proposed)) =
  "⚠️  Plan Revision Needed:\n" <>
  "  Issue: " <> issue <> "\n" <>
  "  Discovery: " <> discovery <> "\n" <>
  "  Proposed: " <> proposed


-- | Short outcome for inline display.
showOutcomeShort :: ChildOutcome -> Text
showOutcomeShort (ChildSuccess hash) = "✓ " <> hash
showOutcomeShort (ChildFailure msg details) =
  "✗ " <> msg <> maybe "" (\d -> " (details: " <> d <> ")") details
showOutcomeShort (ChildPlanRevision issue discovery proposed) =
  "⚠️  " <> issue <> " | " <> discovery <> " | " <> proposed


-- | Show a ChildId in short form.
showChildId :: ChildId -> Text
showChildId (ChildId uuid) = T.take 8 (T.pack (show uuid))


-- | Format a short delta (for inline display).
formatDeltaShort :: NominalDiffTime -> Text
formatDeltaShort secs
  | totalSecs < 60 = T.pack (show totalSecs) <> "s"
  | totalSecs < 3600 = T.pack (show (totalSecs `div` 60)) <> "m"
  | otherwise = T.pack (show (totalSecs `div` 3600)) <> "h"
  where
    totalSecs = round secs :: Int


-- ════════════════════════════════════════════════════════════════════════════
-- ISSUES DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Render detected issues (duplicates, boundary overlaps).
renderIssues :: RunTree -> Text
renderIssues tree =
  let -- Collect directives from spawn events (more accurate than tree structure)
      directives = collectSpawnDirectives tree.rtRoot
      duplicateDirs = filter ((> 1) . length) $ group $ sort directives

      -- Collect boundaries from spawn events
      boundaries = collectSpawnBoundaries tree.rtRoot
      duplicateBounds = filter ((> 1) . length) $ group $ sort $ mapMaybe id boundaries

      allIssues = map showDuplicate duplicateDirs
              <> map showBoundaryOverlap duplicateBounds
  in if null allIssues
     then "✓ No issues detected"
     else T.unlines $ "⚠️  ISSUES:" : allIssues


-- | Collect directives from EvChildSpawned events.
collectSpawnDirectives :: Node -> [Text]
collectSpawnDirectives node =
  let fromEvents = mapMaybe getSpawnDirective (map (.tValue) node.nEvents)
      fromChildren = concatMap collectSpawnDirectives node.nChildren
  in fromEvents <> fromChildren
  where
    getSpawnDirective (EvChildSpawned _ cs) = Just cs.csDirective
    getSpawnDirective _ = Nothing


-- | Collect boundaries from EvChildSpawned events.
collectSpawnBoundaries :: Node -> [Maybe Text]
collectSpawnBoundaries node =
  let fromEvents = mapMaybe getSpawnBoundary (map (.tValue) node.nEvents)
      fromChildren = concatMap collectSpawnBoundaries node.nChildren
  in fromEvents <> fromChildren
  where
    getSpawnBoundary (EvChildSpawned _ cs) = Just cs.csBoundary
    getSpawnBoundary _ = Nothing


-- | Show a duplicate directive issue.
showDuplicate :: [Text] -> Text
showDuplicate [] = ""
showDuplicate (d:ds) =
  "  • Duplicate directive: \"" <> T.take 50 d <> "\" (" <> T.pack (show (1 + length ds)) <> " children)"


-- | Show a boundary overlap issue.
showBoundaryOverlap :: [Text] -> Text
showBoundaryOverlap [] = ""
showBoundaryOverlap (b:bs) =
  "  • Boundary overlap: \"" <> b <> "\" (" <> T.pack (show (1 + length bs)) <> " children target same path)"


-- ════════════════════════════════════════════════════════════════════════════
-- STATS
-- ════════════════════════════════════════════════════════════════════════════

-- | Render run statistics.
renderStats :: RunTree -> Text
renderStats tree =
  let duration = case tree.rtEndTime of
        Just end -> formatDuration (diffUTCTime end tree.rtStartTime)
        Nothing -> "running"
      (succeeded, failed, total) = countChildrenFromEvents tree.rtRoot
      finalCommit = case tree.rtRoot.nOutcome of
        Just (ChildSuccess h) -> h
        _ -> "pending"
      totalCost = sumCosts tree.rtRoot
      costText = if totalCost > 0
                 then " │ $" <> T.pack (printf "%.3f" totalCost)
                 else ""
  in "STATS: " <> duration
     <> " │ " <> T.pack (show succeeded) <> "/" <> T.pack (show total) <> " children"
     <> (if failed > 0 then " (" <> T.pack (show failed) <> " failed)" else "")
     <> costText
     <> " │ " <> finalCommit


-- | Count (succeeded, failed, total) from EvChildComplete events.
countChildrenFromEvents :: Node -> (Int, Int, Int)
countChildrenFromEvents node =
  let -- Count from this node's events
      completions = mapMaybe getCompletion (map (.tValue) node.nEvents)
      thisSucceeded = length $ filter isSuccess completions
      thisFailed = length $ filter (not . isSuccess) completions
      thisTotal = length completions

      -- Recurse into children
      childCounts = map countChildrenFromEvents node.nChildren
      (childSucceeded, childFailed, childTotal) =
        foldr (\(s, f, t) (s', f', t') -> (s+s', f+f', t+t')) (0, 0, 0) childCounts

  in (thisSucceeded + childSucceeded, thisFailed + childFailed, thisTotal + childTotal)
  where
    getCompletion (EvChildComplete _ _ outcome) = Just outcome
    getCompletion _ = Nothing
    isSuccess (ChildSuccess _) = True
    isSuccess (ChildFailure _ _) = False
    isSuccess (ChildPlanRevision _ _ _) = False  -- Plan revision counts as not-success


-- | Sum total cost from all nodes.
sumCosts :: Node -> Double
sumCosts node =
  let thisCost = maybe 0 id node.nTotalCost
      childCosts = sum $ map sumCosts node.nChildren
  in thisCost + childCosts


-- ════════════════════════════════════════════════════════════════════════════
-- OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Write RunTree to files (human-readable + JSON).
writeRunTree :: FilePath -> RunTree -> IO ()
writeRunTree dir tree = do
  TIO.writeFile (dir </> "run-summary.txt") (renderRunTree tree)
  BSL.writeFile (dir </> "run-tree.json") (encode tree)


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Format a UTCTime for display.
formatTimeText :: UTCTime -> Text
formatTimeText = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"


-- | Format a duration nicely.
formatDuration :: (RealFrac a) => a -> Text
formatDuration secs
  | totalSecs < 60 = T.pack (show totalSecs) <> "s"
  | totalSecs < 3600 = T.pack (show mins) <> "m" <> T.pack (show remSecs) <> "s"
  | otherwise = T.pack (show hours) <> "h" <> T.pack (show remMins) <> "m"
  where
    totalSecs = round secs :: Int
    mins = totalSecs `div` 60
    remSecs = totalSecs `mod` 60
    hours = totalSecs `div` 3600
    remMins = (totalSecs `mod` 3600) `div` 60
