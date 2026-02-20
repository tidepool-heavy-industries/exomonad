{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module AnswerEffect where

import Control.Monad.Freer

-- Per-tool input effect (tag 0 in HList)
data AnswerInput' a where
  GetToolInput :: AnswerInput' AnswerInput

data AnswerInput = AnswerInput
  { aiAgentId    :: String
  , aiQuestionId :: String
  , aiAnswer     :: String
  }

-- Shared Inbox effect (tag 1 in HList)
data Inbox a where
  WriteMessage :: String -> String -> String -> String -> Inbox String

-- Shared Questions effect (tag 2 in HList)
data Questions a where
  ResolveQuestion :: String -> String -> Questions String

-- Result returned to caller.
data AnswerResult = AnswerResult
  { arStatus     :: String
  , arAgentId    :: String
  , arQuestionId :: String
  }

-- Entry point: multi-effect composition.
-- Inbox routing and question resolution logic in Haskell.
answerTool :: Eff '[AnswerInput', Inbox, Questions] AnswerResult
answerTool = do
  input <- send GetToolInput
  let agentId = aiAgentId input
  let questionId = aiQuestionId input
  let answer = aiAnswer input
  -- Write answer to the requesting agent's inbox
  _ <- send (WriteMessage agentId "team-lead" answer ("Answer to " ++ questionId))
  -- Resolve the question in the registry
  _ <- send (ResolveQuestion questionId answer)
  pure AnswerResult
    { arStatus = "answered"
    , arAgentId = agentId
    , arQuestionId = questionId
    }
