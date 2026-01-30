-- | GHCi Oracle server entry point.
--
-- Usage:
--   ghci-oracle --port 9999 --project /path/to/project --module Lib
--
-- The server maintains a persistent GHCi session and exposes it via
-- a socket interface for type queries, expression evaluation, etc.
module Main (main) where

import GHCi.Oracle.Server (runServer)
import GHCi.Oracle.Types (OracleConfig (..), defaultConfig)
import Options.Applicative

main :: IO ()
main = do
  config <- execParser opts
  runServer config
  where
    opts =
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc "GHCi Oracle server - persistent GHCi session over sockets"
            <> header "ghci-oracle - a GHCi type query server"
        )

-- ════════════════════════════════════════════════════════════════════════════
-- ARGUMENT PARSING
-- ════════════════════════════════════════════════════════════════════════════

configParser :: Parser OracleConfig
configParser =
  OracleConfig
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value (ocPort defaultConfig)
          <> showDefault
          <> help "Port to listen on"
      )
    <*> strOption
      ( long "project"
          <> short 'd'
          <> metavar "DIR"
          <> value (ocProjectRoot defaultConfig)
          <> showDefault
          <> help "Project root directory"
      )
    <*> many
      ( strOption
          ( long "module"
              <> short 'm'
              <> metavar "MODULE"
              <> help "Module to load at startup (can be repeated)"
          )
      )
    <*> strOption
      ( long "ghci-command"
          <> metavar "CMD"
          <> value (ocGhciCommand defaultConfig)
          <> showDefault
          <> help "Command to start GHCi"
      )
    <*> option
      auto
      ( long "query-timeout"
          <> metavar "MS"
          <> value (ocQueryTimeoutMs defaultConfig)
          <> showDefault
          <> help "Query timeout in milliseconds"
      )
    <*> option
      auto
      ( long "startup-timeout"
          <> metavar "MS"
          <> value (ocStartupTimeoutMs defaultConfig)
          <> showDefault
          <> help "Startup timeout in milliseconds"
      )
    <*> flag
      True
      False
      ( long "no-restart"
          <> help "Disable auto-restart on crash"
      )
    <*> option
      auto
      ( long "max-restarts"
          <> metavar "N"
          <> value (ocMaxRestarts defaultConfig)
          <> showDefault
          <> help "Maximum restart attempts"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose logging"
      )
