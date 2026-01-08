-- | Socket server for GHCi Oracle.
--
-- Accepts connections and dispatches requests to the GHCi session.
module GHCi.Oracle.Server
  ( -- * Server
    runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forever, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
  ( Socket
  , AddrInfo(..)
  , SocketType(..)
  , SocketOption(..)
  , accept
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , listen
  , setSocketOption
  , socket
  , withSocketsDo
  )

import GHCi.Oracle.Protocol
import GHCi.Oracle.Session
import GHCi.Oracle.Types


-- ════════════════════════════════════════════════════════════════════════════
-- SERVER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the oracle server.
--
-- 1. Start GHCi session
-- 2. Listen on configured port
-- 3. Accept connections and handle requests
runServer :: OracleConfig -> IO ()
runServer config = withSocketsDo $ do
  -- Create session handle
  handle <- newSessionHandle config

  -- Start GHCi session
  logInfo config "Starting GHCi session..."
  startResult <- startSession handle
  case startResult of
    Left err -> do
      logError config $ "Failed to start GHCi session: " <> T.pack (show err)
      pure ()
    Right () -> do
      logInfo config "GHCi session started successfully"

      -- Create listening socket
      bracket (createServerSocket config) close $ \sock -> do
        logInfo config $ "Listening on port " <> T.pack (show $ ocPort config)

        -- Accept loop
        forever $ do
          (clientSock, _) <- accept sock
          logInfo config "Client connected"
          -- Handle client in separate thread
          void $ forkIO $ handleClient config handle clientSock


-- | Create and configure server socket.
createServerSocket :: OracleConfig -> IO Socket
createServerSocket config = do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show $ ocPort config)
  addr <- case addrs of
    (a:_) -> pure a
    []    -> ioError (userError "getAddrInfo: no addresses for 127.0.0.1")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 5
  pure sock


-- | Handle a client connection.
handleClient :: OracleConfig -> SessionHandle -> Socket -> IO ()
handleClient config handle sock = do
  handleRequests `catch` handleError `finally` cleanup
  where
    handleRequests = forever $ do
      reqResult <- receiveRequest sock
      case reqResult of
        Left err -> do
          logError config $ "Receive error: " <> err
          -- Send error response and continue
          sendResponse sock $ RespError $ GHCiServerError err
        Right req -> do
          when (ocVerbose config) $
            logInfo config $ "Request: " <> T.pack (show req)
          resp <- handleRequest handle req
          when (ocVerbose config) $
            logInfo config $ "Response: " <> T.pack (show resp)
          sendResponse sock resp

    handleError (e :: SomeException) =
      logError config $ "Client error: " <> T.pack (show e)

    cleanup = do
      logInfo config "Client disconnected"
      close sock


-- | Handle a single request.
handleRequest :: SessionHandle -> GHCiRequest -> IO GHCiResponse
handleRequest handle = \case
  ReqPing -> pure RespPong

  ReqQueryType expr -> runQuery handle (":type " <> expr) $ \out ->
    RespSuccess (cleanTypeOutput out)

  ReqQueryInfo name -> runQuery handle (":info " <> name) $ \out ->
    RespSuccess out

  ReqQueryKind typ -> runQuery handle (":kind " <> typ) $ \out ->
    RespSuccess (cleanKindOutput out)

  ReqEvaluate expr -> runQuery handle expr $ \out ->
    RespSuccess out

  ReqCheckCompiles expr -> do
    result <- runQueryRaw handle (":type " <> expr)
    pure $ case result of
      Left (GHCiParseError _ _) -> RespBool False
      Left err -> RespError err
      Right _ -> RespBool True

  ReqLoadModule modName -> do
    result <- runQueryRaw handle (":load " <> modName)
    pure $ case result of
      Left err -> RespError err
      Right out
        | isLoadError out -> RespError $ GHCiLoadError modName out
        | otherwise -> RespUnit

  ReqReloadModules -> do
    result <- runQueryRaw handle ":reload"
    pure $ case result of
      Left err -> RespError err
      Right out
        | isLoadError out -> RespError $ GHCiLoadError "*" out
        | otherwise -> RespUnit


-- | Run a query and transform successful result.
runQuery :: SessionHandle -> Text -> (Text -> GHCiResponse) -> IO GHCiResponse
runQuery handle query onSuccess = do
  result <- runQueryRaw handle query
  pure $ case result of
    Left err -> RespError err
    Right out -> onSuccess out


-- | Run a raw query, returning Either.
-- withSession passes through the Either from execQuery directly.
runQueryRaw :: SessionHandle -> Text -> IO (Either GHCiError Text)
runQueryRaw handle query =
  withSession handle $ \session ->
    execQuery handle session query


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Clean up :type output (remove leading "expr :: ").
cleanTypeOutput :: Text -> Text
cleanTypeOutput t =
  case T.breakOn " :: " t of
    (_, rest) | not (T.null rest) -> T.drop 4 rest  -- Drop " :: "
    _ -> t


-- | Clean up :kind output (remove leading "type :: ").
cleanKindOutput :: Text -> Text
cleanKindOutput t =
  case T.breakOn " :: " t of
    (_, rest) | not (T.null rest) -> T.drop 4 rest
    _ -> t


-- | Log info message.
logInfo :: OracleConfig -> Text -> IO ()
logInfo _config msg = TIO.putStrLn $ "[ghci-oracle] " <> msg


-- | Log error message.
logError :: OracleConfig -> Text -> IO ()
logError _ msg = TIO.putStrLn $ "[ghci-oracle ERROR] " <> msg
