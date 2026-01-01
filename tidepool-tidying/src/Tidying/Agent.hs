-- | Tidying Agent
--
-- The Tidying agent owns its full lifecycle via 'tidyingRun'.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with pure routing.
--
-- = Graph-Based Implementation
--
-- The agent can use either the manual loop ('tidyingTurn') or the
-- graph-based dispatch ('tidyingTurnGraph'). Both produce identical
-- behavior, but the graph version uses the V2 Graph DSL for typed
-- dispatch through nodes.
--
-- To switch to graph-based:
--
-- @
-- -- In tidyingRun, replace:
-- response <- tidyingTurn userInput
-- -- With:
-- response <- tidyingTurnGraph userInput
-- @
--
-- = Photo Support
--
-- Photos are attached to text input in the GUI (chat-app style).
-- The GUI sends base64-encoded photos directly - no filesystem involved.
-- Users can attach a photo while typing their message.
--
-- = Tool-Based Flow
--
-- The LLM uses tools to interact with the user mid-turn:
--
-- 1. LLM analyzes photo, sees "blue mug on papers"
-- 2. LLM calls 'propose_disposition' with ranked choices
-- 3. Tool presents choices to user via QuestionUI effect
-- 4. User taps to confirm or types correction
-- 5. Answer returns to LLM, which continues processing
--
-- This requires the 'QuestionUI' effect in the stack.
--
module Tidying.Agent
  ( -- * Agent
    tidying
  , TidyingM
  , TidyingExtra
  , tidyingRun
  ) where

import Control.Monad (unless)
import qualified Data.Text as T

import Tidepool
import Tidying.State (SessionState, newSession, UserInput(..), Photo(..))
import Tidying.Loop (tidyingTurn, TidyingEvent(..), Response(..))
import Tidying.Graph.Handlers (tidyingTurnGraph)

-- | Extra effects for Tidying agent
--
-- The QuestionUI effect enables mid-turn tools like 'propose_disposition'
-- that pause for user input.
type TidyingExtra = '[QuestionUI]

-- | The Tidying agent monad
type TidyingM = AgentM SessionState TidyingEvent TidyingExtra

-- | The Tidying agent
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Uses OODA (Observe-Orient-Decide-Act) pattern with tool-based user interaction.
--
-- Tools available:
--
-- * 'propose_disposition' - LLM proposes where item goes, user confirms
-- * 'ask_space_function' - LLM asks what the space is for
-- * 'confirm_done' - confirm session is complete
tidying :: Agent SessionState TidyingEvent TidyingExtra
tidying = Agent
  { agentName       = "tidying"
  , agentInit       = newSession
  , agentRun        = tidyingRun
  , agentDispatcher = noDispatcher  -- Actual dispatcher wired in Runner.hs via runLLMWithToolsHooked
  }

-- | The Tidying agent's main loop
--
-- Supports photo attachments via requestTextWithPhoto.
-- Photos are converted to base64 and sent with the text input.
tidyingRun :: TidyingM ()
tidyingRun = do
  -- Startup
  emit $ SituationClassified "Let's tidy! Tell me about your space."
  emit $ SituationClassified "You can attach photos of your space for analysis."

  -- Main loop
  loop

  -- Shutdown
  emit $ SituationClassified "Great work! Session complete."
  where
    loop = do
      (input, photoData) <- requestTextWithPhoto "> "
      let trimmed = T.toLower (T.strip input)
          hasPhoto = not (null photoData)
          hasText = not (T.null trimmed)

      -- Skip empty input (no text, no photo)
      if not hasText && not hasPhoto
        then loop
        else do
          -- Convert photo data tuples to Photo values
          let photos = map (\(b64, mime) -> Photo b64 mime) photoData

          -- Emit user input for chat display (with photo indicator)
          let photoIndicator = if hasPhoto then " [📷]" else ""
          emit $ UserInputReceived (input <> photoIndicator)

          let userInput = UserInput
                { inputText = if hasText then Just input else Nothing
                , inputPhotos = photos
                }
          -- Use tidyingTurn (manual OODA loop) or tidyingTurnGraph (V2 graph DSL)
          -- Both produce identical behavior
          response <- tidyingTurn userInput
          -- Alternative: response <- tidyingTurnGraph userInput

          -- Emit response for chat display
          emit $ ResponseGenerated response.responseText

          -- Exit if session ended (quit/done/stop), otherwise continue
          unless response.responseSessionEnded loop
