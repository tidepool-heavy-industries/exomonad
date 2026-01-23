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
import Tidepool.Control.LSPTools
  ( FindCallersGraph, ShowFieldsGraph, ShowConstructorsGraph )
import Tidepool.Control.TUITools
  ( ConfirmActionGraph, SelectOptionGraph, RequestGuidanceGraph )
import Tidepool.Control.FeedbackTools
  ( RegisterFeedbackGraph )
import Tidepool.Control.ExoTools
  ( ExoStatusGraph, ExoCompleteGraph, ExoReconstituteGraph, SpawnAgentsGraph, FilePRGraph, PrReviewStatusGraph )
import Tidepool.Control.PMTools
  ( PmApproveExpansionGraph, PmPrioritizeGraph )
import Tidepool.Control.PMReviewDAG (PmReviewDagGraph)
import Tidepool.Control.PMStatus (PmStatusGraph)
import Tidepool.Control.PMPropose (PMProposeGraph)
import Tidepool.Control.MailboxTools
  ( SendMessageGraph, CheckInboxGraph, ReadMessageGraph, MarkReadGraph )
import Tidepool.Control.Scout.Graph (DocGenGraph)

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
  let fcTools = reifyGraphEntries (Proxy @FindCallersGraph)
  let sfTools = reifyGraphEntries (Proxy @ShowFieldsGraph)
  let scTools = reifyGraphEntries (Proxy @ShowConstructorsGraph)
  let caTools = reifyGraphEntries (Proxy @ConfirmActionGraph)
  let soTools = reifyGraphEntries (Proxy @SelectOptionGraph)
  let rgTools = reifyGraphEntries (Proxy @RequestGuidanceGraph)
  let rfTools = reifyMCPTools (Proxy @RegisterFeedbackGraph)

  -- Extract tools from complex graphs via MCPExport (legacy pattern)
  let dgTools = reifyMCPTools (Proxy @DocGenGraph)
  let esTools = reifyMCPTools (Proxy @ExoStatusGraph)
  let ecTools = reifyMCPTools (Proxy @ExoCompleteGraph)
  let erTools = reifyMCPTools (Proxy @ExoReconstituteGraph)
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

  -- Log discovered tools per graph for debugging
  logDebug logger $ "[MCP Discovery] FindCallersGraph: " <> T.pack (show (length fcTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ShowFieldsGraph: " <> T.pack (show (length sfTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ShowConstructorsGraph: " <> T.pack (show (length scTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ConfirmActionGraph: " <> T.pack (show (length caTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] SelectOptionGraph: " <> T.pack (show (length soTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] RequestGuidanceGraph: " <> T.pack (show (length rgTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] RegisterFeedbackGraph: " <> T.pack (show (length rfTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] DocGenGraph: " <> T.pack (show (length dgTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ExoStatusGraph: " <> T.pack (show (length esTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ExoCompleteGraph: " <> T.pack (show (length ecTools)) <> " tools"
  logDebug logger $ "[MCP Discovery] ExoReconstituteGraph: " <> T.pack (show (length erTools)) <> " tools"
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

  let allTools = concat [fcTools, sfTools, scTools, caTools, soTools, rgTools, rfTools, dgTools, esTools, ecTools, erTools, saTools, fpTools, paeTools, pmPriTools, pmRevTools, pmStatTools, pmProTools, prTools, smTools, ciTools, rmTools, mrTools]
  logInfo logger $ "[MCP Discovery] Total: " <> T.pack (show (length allTools)) <> " tools discovered"

  -- Log tool names with entry points for verification
  forM_ allTools $ \(MCPToolInfo name _desc _schema entryName) -> 
    logDebug logger $ "[MCP Discovery]   " <> name <> " -> " <> entryName

  logDebug logger "[MCP Discovery] Converting to ToolDefinition format..."
  pure $ map reifyToToolDef allTools

-- | Convert MCPToolInfo -> ToolDefinition.
reifyToToolDef :: MCPToolInfo -> ToolDefinition
reifyToToolDef (MCPToolInfo name desc schema _entryName) = ToolDefinition 
  { tdName = name 
  , tdDescription = desc 
  , tdInputSchema = schema 
  }
