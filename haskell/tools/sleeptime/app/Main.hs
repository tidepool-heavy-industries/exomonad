{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SleeptimeLogs.CLI
import SleeptimeLogs.Config
import SleeptimeLogs.Loki
import SleeptimeLogs.Output

main :: IO ()
main = do
  opts <- parseOptions
  cfg <- loadConfig

  result <- runCommand cfg opts.optCommand

  if opts.optJson
    then formatResultJson result
    else formatResult result

runCommand :: Config -> Command -> IO QueryResult
runCommand cfg = \case
  Transitions{..} ->
    queryTransitions cfg since.durationSeconds limit

  LLMCalls{..} ->
    queryLLMCalls cfg minLatency since.durationSeconds limit

  Errors{..} ->
    queryErrors cfg since.durationSeconds limit

  Session{..} ->
    querySession cfg sessionId limit
