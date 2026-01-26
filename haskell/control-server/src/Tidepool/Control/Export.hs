{-# LANGUAGE OverloadedStrings #-}

-- | Export MCP tools from graph DSL annotations.
module Tidepool.Control.Export
  ( exportMCPTools
  ) where

import Control.Monad (forM_)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.Logging (Logger, logInfo, logDebug)
import Tidepool.Control.Protocol (ToolDefinition(..))

-- Tool definitions from Graph DSL
import Tidepool.Control.TUITools
  ( ConfirmActionGraph, SelectOptionGraph, RequestGuidanceGraph )
import Tidepool.Control.FeedbackTools
  ( RegisterFeedbackGraph )
import Tidepool.Control.ExoTools
  ( ExoStatusGraph, SpawnAgentsGraph, FilePRGraph, PrReviewStatusGraph )
import Tidepool.Control.PMTools
  ( PmApproveExpansionGraph, PmPrioritizeGraph )
import Tidepool.Control.PMReviewDAG (PmReviewDagGraph)
import Tidepool.Control.PMStatus (PmStatusGraph)
import Tidepool.Control.PMPropose (PMProposeGraph)
import Tidepool.Control.MailboxTools
  ( SendMessageGraph, CheckInboxGraph, ReadMessageGraph, MarkReadGraph )
import Tidepool.Control.BDTools
  ( BDListGraph, BDShowGraph, BDReadyGraph, BDCreateGraph
  , BDUpdateGraph, BDCloseGraph, BDAddDepGraph, BDAddLabelGraph
  )

import Tidepool.Graph.MCPReify (ReifyMCPTools(..), ReifyGraphEntries(..), MCPToolInfo(..))

-- | Export all MCP tools from graph DSL annotations.
--
-- Uses ReifyGraphEntries (new) for simplified graphs with GraphEntries,
-- and ReifyMCPTools (legacy) for graphs with MCPExport annotations.
-- Returns ToolDefinition format that matches Rust protocol types.
exportMCPTools :: Logger -> IO [ToolDefinition]
exportMCPTools logger = do 
  logInfo logger "[MCP Discovery] Starting tool discovery from graphs..."

  -- Extract tools from simplified graphs via GraphEntries (new pattern)
  -- Note: LSP tools (FindCallersGraph etc) removed as they require LSP session
  let caTools = reifyGraphEntries (Proxy @ConfirmActionGraph)
  let soTools = reifyGraphEntries (Proxy @SelectOptionGraph)
  let rgTools = reifyGraphEntries (Proxy @RequestGuidanceGraph)
  let rfTools = reifyMCPTools (Proxy @RegisterFeedbackGraph)

  -- Extract tools from complex graphs via MCPExport (legacy pattern)
  -- Note: DocGenGraph removed as it requires LSP
  -- Note: ExoCompleteGraph and PreCommitCheckGraph folded into Stop hook
  let esTools = reifyMCPTools (Proxy @ExoStatusGraph)
  let saTools = reifyMCPTools (Proxy @SpawnAgentsGraph)
  let fpTools = reifyMCPTools (Proxy @FilePRGraph)
  let paeTools = reifyMCPTools (Proxy @PmApproveExpansionGraph)
  let pmPriTools = reifyMCPTools (Proxy @PmPrioritizeGraph)
  let pmRevTools = reifyMCPTools (Proxy @PmReviewDagGraph)
  let pmStatTools = reifyMCPTools (Proxy @PmStatusGraph)
  let pmProTools = reifyMCPTools (Proxy @PMProposeGraph)
  let prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)

  -- Mailbox tools
  let smTools = reifyMCPTools (Proxy @SendMessageGraph)
  let ciTools = reifyMCPTools (Proxy @CheckInboxGraph)
  let rmTools = reifyMCPTools (Proxy @ReadMessageGraph)
  let mrTools = reifyMCPTools (Proxy @MarkReadGraph)

  -- BD (Beads) tools
  let bdListTools = reifyMCPTools (Proxy @BDListGraph)
  let bdShowTools = reifyMCPTools (Proxy @BDShowGraph)
  let bdReadyTools = reifyMCPTools (Proxy @BDReadyGraph)
  let bdCreateTools = reifyMCPTools (Proxy @BDCreateGraph)
  let bdUpdateTools = reifyMCPTools (Proxy @BDUpdateGraph)
  let bdCloseTools = reifyMCPTools (Proxy @BDCloseGraph)
  let bdAddDepTools = reifyMCPTools (Proxy @BDAddDepGraph)
  let bdAddLabelTools = reifyMCPTools (Proxy @BDAddLabelGraph)

  -- Log discovered tools per graph for debugging
  logDebug logger $ "[MCP Discovery] ConfirmActionGraph: " <> T.pack (show (length caTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] SelectOptionGraph: " <> T.pack (show (length soTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] RequestGuidanceGraph: " <> T.pack (show (length rgTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] RegisterFeedbackGraph: " <> T.pack (show (length rfTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ExoStatusGraph: " <> T.pack (show (length esTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] SpawnAgentsGraph: " <> T.pack (show (length saTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] FilePRGraph: " <> T.pack (show (length fpTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PmApproveExpansionGraph: " <> T.pack (show (length paeTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PmPrioritizeGraph: " <> T.pack (show (length pmPriTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PmReviewDagGraph: " <> T.pack (show (length pmRevTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PmStatusGraph: " <> T.pack (show (length pmStatTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PMProposeGraph: " <> T.pack (show (length pmProTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PrReviewStatusGraph: " <> T.pack (show (length prTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] SendMessageGraph: " <> T.pack (show (length smTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] CheckInboxGraph: " <> T.pack (show (length ciTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ReadMessageGraph: " <> T.pack (show (length rmTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] MarkReadGraph: " <> T.pack (show (length mrTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDListGraph: " <> T.pack (show (length bdListTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDShowGraph: " <> T.pack (show (length bdShowTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDReadyGraph: " <> T.pack (show (length bdReadyTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDCreateGraph: " <> T.pack (show (length bdCreateTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDUpdateGraph: " <> T.pack (show (length bdUpdateTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDCloseGraph: " <> T.pack (show (length bdCloseTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDAddDepGraph: " <> T.pack (show (length bdAddDepTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] BDAddLabelGraph: " <> T.pack (show (length bdAddLabelTools)) <> " tools"

  let allTools = concat
        [ caTools, soTools, rgTools, rfTools
        , esTools, saTools, fpTools, paeTools, pmPriTools, pmRevTools, pmStatTools, pmProTools, prTools
        , smTools, ciTools, rmTools, mrTools
        , bdListTools, bdShowTools, bdReadyTools, bdCreateTools, bdUpdateTools, bdCloseTools, bdAddDepTools, bdAddLabelTools
        ]
  logInfo logger $ "[MCP Discovery] Total: " <> T.pack (show (length allTools)) <> " tools discovered"

  -- Log tool names with entry points for verification
  forM_ allTools $ \(MCPToolInfo name _desc _schema entryName _roles) -> 
    logDebug logger $ "[MCP Discovery]   " <> name <> " -> " <> entryName

  logDebug logger "[MCP Discovery] Converting to ToolDefinition format..."
  pure $ map reifyToToolDef allTools

-- | Convert MCPToolInfo -> ToolDefinition.
reifyToToolDef :: MCPToolInfo -> ToolDefinition
reifyToToolDef (MCPToolInfo name desc schema _entryName _roles) = ToolDefinition 
  { tdName = name 
  , tdDescription = desc 
  , tdInputSchema = schema 
  }
