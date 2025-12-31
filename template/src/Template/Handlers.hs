{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the graph.
--
-- Each node in the graph has a corresponding handler here.
module Template.Handlers
  ( simpleHandlers
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T

import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (LLMHandler(..), gotoExit)

import Template.Context (ProcessContext(..))
import Template.Graph (SimpleGraph(..), Input(..), Output(..), Result(..))

-- | Handlers for SimpleGraph.
--
-- - sgProcess: LLMBefore builds template context from Input
-- - sgRoute: Routes LLM output to Exit
simpleHandlers :: SimpleGraph (AsHandler '[])
simpleHandlers = SimpleGraph
  { sgEntry   = Proxy @Input

    -- LLMBefore: Build template context, then runner handles LLM call
    -- The template (process.jinja) receives this context for rendering
  , sgProcess = LLMBefore $ \input -> do
      pure ProcessContext
        { input = T.pack input.inputText
        }

    -- Logic handler: Routes based on LLM output
  , sgRoute   = \output -> pure $ gotoExit (Result output.outputText)

  , sgExit    = Proxy @Result
  }
