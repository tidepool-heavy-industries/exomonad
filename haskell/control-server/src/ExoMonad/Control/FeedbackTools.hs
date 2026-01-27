{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MCP tools for capturing subagent experience and feedback.
module ExoMonad.Control.FeedbackTools
  ( -- * Register Feedback
    RegisterFeedbackGraph(..)
  , registerFeedbackLogic
  , RegisterFeedbackArgs(..)
  , RegisterFeedbackResult(..)
  , TokenCategoryEstimate(..)
  ) where

import Control.Exception (try, SomeException, displayException)
import Control.Monad.Freer (Eff, sendM, LastMember)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import ExoMonad.Graph.Generic (type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)

import ExoMonad.Control.FeedbackTools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for register_feedback tool.
data RegisterFeedbackGraph mode = RegisterFeedbackGraph
  { rfEntry :: mode :- EntryNode RegisterFeedbackArgs
      :@ MCPExport
      :@ MCPToolDef '("register_feedback", "Capture subagent experience and suggestions for system improvement. Before calling this, review your session: What tools did you use most? What was missing? What friction did you encounter?")

  , rfRun :: mode :- LogicNode
      :@ Input RegisterFeedbackArgs
      :@ UsesEffects '[Goto Exit RegisterFeedbackResult]

  , rfExit :: mode :- ExitNode RegisterFeedbackResult
  }
  deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for register_feedback.
--
-- Saves the feedback to .exomonad/feedback/<issue_id>.json.
registerFeedbackLogic
  :: LastMember IO es
  => RegisterFeedbackArgs
  -> Eff es (GotoChoice '[To Exit RegisterFeedbackResult])
registerFeedbackLogic args = do
  let relativePath = ".exomonad" </> "feedback" </> T.unpack args.rfaIssueId <> ".json"
  
  -- Perform IO to write the file
  result <- sendM $ try $ do
    createDirectoryIfMissing True (takeDirectory relativePath)
    LBS.writeFile relativePath (encode args)
    pure relativePath

  case result of
    Left (e :: SomeException) -> 
      pure $ gotoExit RegisterFeedbackResult 
        { rfrSuccess = False
        , rfrPath = ""
        , rfrError = Just $ T.pack $ displayException e
        }
    Right path ->
      pure $ gotoExit RegisterFeedbackResult 
        { rfrSuccess = True 
        , rfrPath = T.pack path
        , rfrError = Nothing
        }
