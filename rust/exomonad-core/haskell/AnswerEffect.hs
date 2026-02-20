{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
module AnswerEffect where

import Control.Monad.Freer

-- Effect GADT. Constructor names must match Rust #[core(name = "...")] exactly.
data AnswerOp a where
  GetToolInput   :: AnswerOp AnswerInput
  AnswerQuestion :: String -> String -> String -> AnswerOp AnswerResult

-- Input from MCP args.
data AnswerInput = AnswerInput
  { aiAgentId    :: String
  , aiQuestionId :: String
  , aiAnswer     :: String
  }

-- Result returned to caller.
data AnswerResult = AnswerResult
  { arStatus     :: String
  , arAgentId    :: String
  , arQuestionId :: String
  }

-- Entry point: zero-arg, all input via effects.
answerTool :: Eff '[AnswerOp] AnswerResult
answerTool = do
  input <- send GetToolInput
  send (AnswerQuestion (aiAgentId input) (aiQuestionId input) (aiAnswer input))
