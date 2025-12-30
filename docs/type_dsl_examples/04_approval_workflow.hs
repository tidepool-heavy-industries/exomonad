{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Human-in-the-Loop Approval Workflow
--
-- AI drafts, human reviews, loop until approved.
-- Demonstrates: RequestInput effect, revision loops via Goto, When conditional, Vision.
--
-- ```mermaid
-- flowchart TD
--     entry((start)) -->|Request| analyzeAttachments
--     entry -->|Request| draft
--     analyzeAttachments[["analyzeAttachments<br/>LLM · Vision"]] -.->|"Maybe AttachmentAnalysis"| draft
--     draft[["draft<br/>LLM"]] -->|DraftResponse| review
--     review{{"review<br/>Logic + RequestInput"}} -->|Approval| handle
--     handle{{"handle<br/>Logic"}} -->|FinalResult| exit((end))
--     handle -->|RevisionContext| revise
--     revise[["revise<br/>LLM"]] -->|DraftResponse| review
-- ```

module Examples.ApprovalWorkflow where

import Tidepool.Graph

-- ════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════

data Request = Request
  { reqType :: Text
  , reqContext :: Text
  , reqInstructions :: Text
  , reqAttachments :: [Attachment]
  }

data Attachment = Attachment
  { attName :: Text
  , attMimeType :: Text
  , attData :: ByteString
  }

data AttachmentAnalysis = AttachmentAnalysis
  { aaDescriptions :: [Text]
  , aaRelevantDetails :: [Text]
  , aaSuggestedInclusions :: [Text]
  }
  deriving (Generic, FromJSON)

data DraftResponse = DraftResponse
  { drContent :: Text
  , drTone :: Text
  , drKeyPoints :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON)

data Approval
  = Approved Text
  | Rejected Text
  | NeedsRevision Text

data RevisionContext = RevisionContext
  { rvRequest :: Request
  , rvPreviousDraft :: DraftResponse
  , rvFeedback :: Text
  , rvAttachmentAnalysis :: Maybe AttachmentAnalysis
  }

data FinalResult
  = Sent Text
  | Aborted Text

-- Condition for When
data HasAttachments

-- ════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════

type ApprovalGraph = Graph '[
    Entry :~> Request

    -- Analyze attachments (CONDITIONAL - only if attachments present)
  , "analyzeAttachments" := LLM
      :@ Needs '[Request]
      :@ When HasAttachments
      :@ Vision
      :@ Schema AttachmentAnalysis

    -- Initial draft (receives Maybe because upstream is conditional)
  , "draft" := LLM
      :@ Needs '[Request, Maybe AttachmentAnalysis]
      :@ Template DraftTpl
      :@ Schema DraftResponse

    -- Revision (receives from handle's Goto)
  , "revise" := LLM
      :@ Needs '[RevisionContext]
      :@ Template ReviseTpl
      :@ Schema DraftResponse

    -- Human review (blocks for input, then sends to handle)
  , "review" := Logic
      :@ Needs '[DraftResponse]
      :@ Eff '[RequestInput, Goto "handle" Approval]

    -- Handle decision: exit or request revision
  , "handle" := Logic
      :@ Needs '[Request, DraftResponse, Approval, Maybe AttachmentAnalysis]
      :@ Eff '[
          Goto Exit FinalResult
        , Goto "revise" RevisionContext
        ]

  , Exit :<~ FinalResult
  ]

-- Edge derivation:
--   Entry → analyzeAttachments (analyzeAttachments Needs Request)
--   Entry → draft (draft Needs Request)
--   analyzeAttachments -.-> draft (draft Needs Maybe AttachmentAnalysis, conditional)
--   draft → review (review Needs DraftResponse, draft Schema DraftResponse)
--   revise → review (review Needs DraftResponse, revise Schema DraftResponse)
--   review → handle (review Goto "handle" Approval)
--   Entry → handle (handle Needs Request)
--   draft → handle (handle Needs DraftResponse)
--   analyzeAttachments -.-> handle (handle Needs Maybe AttachmentAnalysis)
--   handle → Exit (handle Goto Exit FinalResult)
--   handle → revise (handle Goto "revise" RevisionContext)

-- ════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════

-- Handler types:
--   analyzeAttachments :: Request -> LLM AttachmentAnalysis
--   draft :: Request -> Maybe AttachmentAnalysis -> LLM DraftResponse
--   revise :: RevisionContext -> LLM DraftResponse
--   review :: DraftResponse -> Eff '[RequestInput, Goto "handle" Approval] ()
--   handle :: Request -> DraftResponse -> Approval -> Maybe AttachmentAnalysis -> Eff '[Goto Exit FinalResult, Goto "revise" RevisionContext] ()

approvalHandlers :: Handlers ApprovalGraph
approvalHandlers = Handlers
  { analyzeAttachments = \req ->
      runVision attachmentAnalysisPrompt req.reqAttachments

  , draft = \req mAnalysis ->
      runLLM draftTemplate (req, mAnalysis)

  , revise = \ctx ->
      runLLM reviseTemplate ctx

  , review = \draftResp -> do
      choice <- requestChoice
        "Review this draft:"
        [ ("Approve", "approve")
        , ("Approve with edits", "edit")
        , ("Request revision", "revise")
        , ("Reject", "reject")
        ]
      approval <- case choice of
        "approve" -> pure $ Approved draftResp.drContent
        "edit" -> do
          edited <- requestText "Enter your edited version:"
          pure $ Approved edited
        "revise" -> do
          feedback <- requestText "What should be changed?"
          pure $ NeedsRevision feedback
        "reject" -> do
          reason <- requestText "Reason for rejection:"
          pure $ Rejected reason
      goto @"handle" approval

  , handle = \req draft approval mAnalysis ->
      case approval of
        Approved content ->
          goto @Exit (Sent content)
        Rejected reason ->
          goto @Exit (Aborted reason)
        NeedsRevision feedback ->
          goto @"revise" RevisionContext
            { rvRequest = req
            , rvPreviousDraft = draft
            , rvFeedback = feedback
            , rvAttachmentAnalysis = mAnalysis
            }
  }

-- ════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════

-- This example demonstrates:
--
-- 1. When conditional nodes
--    - analyzeAttachments has: When HasAttachments
--    - Only runs if Request has attachments
--    - If skipped, downstream gets Nothing
--
-- 2. Auto-Maybe for conditional dependencies
--    - analyzeAttachments is conditional
--    - draft Needs Maybe AttachmentAnalysis
--    - handle also Needs Maybe AttachmentAnalysis
--    - Mermaid shows dashed edges (.->) for Maybe
--
-- 3. Vision for image analysis
--    - analyzeAttachments uses Vision instead of Template
--    - Handler uses runVision for image processing
--
-- 4. RequestInput effect
--    - review has RequestInput in its Eff stack
--    - Blocks for human input via requestChoice/requestText
--    - Then sends result to handle via Goto
--
-- 5. Revision loop
--    - handle can Goto "revise" with RevisionContext
--    - revise produces DraftResponse (Schema)
--    - review Needs DraftResponse
--    - Loop: review → handle → revise → review
--
-- 6. Context preservation across loop
--    - RevisionContext includes Maybe AttachmentAnalysis
--    - Attachment analysis done once, reused across revisions
