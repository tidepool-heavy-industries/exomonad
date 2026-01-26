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
import Tidepool.Control.ExoTools ( ExoStatusGraph, SpawnAgentsGraph, FilePRGraph, PrReviewStatusGraph )
import Tidepool.Control.PMTools
  ( PmApproveExpansionGraph, PmPrioritizeGraph )
import Tidepool.Control.PMStatus (PmStatusGraph)
import Tidepool.Control.PMPropose (PMProposeGraph)
import Tidepool.Control.MailboxTools
  ( SendMessageGraph, CheckInboxGraph, ReadMessageGraph, MarkReadGraph )
import Tidepool.Control.GHTools
  ( GHIssueListGraph, GHIssueShowGraph, GHIssueCreateGraph
  , GHIssueUpdateGraph, GHIssueCloseGraph, GHIssueReopenGraph
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
  let pmStatTools = reifyMCPTools (Proxy @PmStatusGraph)
  let pmProTools = reifyMCPTools (Proxy @PMProposeGraph)
  let prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)

  -- Mailbox tools
  let smTools = reifyMCPTools (Proxy @SendMessageGraph)
  let ciTools = reifyMCPTools (Proxy @CheckInboxGraph)
  let rmTools = reifyMCPTools (Proxy @ReadMessageGraph)
  let mrTools = reifyMCPTools (Proxy @MarkReadGraph)

  -- GitHub tools
  let ghListTools = reifyMCPTools (Proxy @GHIssueListGraph)
  let ghShowTools = reifyMCPTools (Proxy @GHIssueShowGraph)
  let ghCreateTools = reifyMCPTools (Proxy @GHIssueCreateGraph)
  let ghUpdateTools = reifyMCPTools (Proxy @GHIssueUpdateGraph)
  let ghCloseTools = reifyMCPTools (Proxy @GHIssueCloseGraph)
  let ghReopenTools = reifyMCPTools (Proxy @GHIssueReopenGraph)

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
  logDebug logger $ "[MCP Discovery] PmStatusGraph: " <> T.pack (show (length pmStatTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PMProposeGraph: " <> T.pack (show (length pmProTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] PrReviewStatusGraph: " <> T.pack (show (length prTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] SendMessageGraph: " <> T.pack (show (length smTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] CheckInboxGraph: " <> T.pack (show (length ciTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ReadMessageGraph: " <> T.pack (show (length rmTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] MarkReadGraph: " <> T.pack (show (length mrTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueListGraph: " <> T.pack (show (length ghListTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueShowGraph: " <> T.pack (show (length ghShowTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueCreateGraph: " <> T.pack (show (length ghCreateTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueUpdateGraph: " <> T.pack (show (length ghUpdateTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueCloseGraph: " <> T.pack (show (length ghCloseTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] GHIssueReopenGraph: " <> T.pack (show (length ghReopenTools)) <> " tools"

  let allTools = concat
        [ caTools, soTools, rgTools, rfTools
        , esTools, saTools, fpTools, paeTools, pmPriTools, pmStatTools, pmProTools, prTools
        , smTools, ciTools, rmTools, mrTools
        , ghListTools, ghShowTools, ghCreateTools, ghUpdateTools, ghCloseTools, ghReopenTools
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
