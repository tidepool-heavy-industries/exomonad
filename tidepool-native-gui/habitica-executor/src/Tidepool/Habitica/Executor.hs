-- | Habitica effect executor - native HTTP client implementation.
--
-- Implements the 'Habitica' effect from "Tidepool.Effects.Habitica" using
-- http-client-tls for direct API calls.
--
-- = Usage
--
-- @
-- import Tidepool.Habitica.Executor (runHabitica, HabiticaConfig(..))
-- import Tidepool.Effects.Habitica (Habitica, fetchTodos)
--
-- config = HabiticaConfig
--   { hcBaseUrl = "https://habitica.com/api/v3"
--   , hcUserId = "your-user-id"
--   , hcApiToken = "your-api-token"
--   }
--
-- main = runHabitica config $ do
--   todos <- fetchTodos
--   -- process todos
-- @
module Tidepool.Habitica.Executor
  ( -- * Executor
    runHabitica

    -- * Configuration
  , HabiticaConfig(..)
  , defaultHabiticaConfig

    -- * Internal (for testing)
  , HabiticaEnv(..)
  , mkHabiticaEnv
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , eitherDecode
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
  ( Manager
  , Request(..)
  , RequestBody(..)
  , Response(..)
  , httpLbs
  , newManager
  , parseRequest
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Tidepool.Effects.Habitica
  ( Habitica(..)
  , HabiticaError(..)
  , TaskType(..)
  , Direction(..)
  , TaskId(..)
  , TodoId(..)
  , ChecklistItemId(..)
  , UserInfo
  , HabiticaTask
  , FetchedTodo
  , ScoreResult
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Habitica API configuration.
data HabiticaConfig = HabiticaConfig
  { hcBaseUrl  :: Text  -- ^ API base URL (default: https://habitica.com/api/v3)
  , hcUserId   :: Text  -- ^ Habitica user ID (x-api-user header)
  , hcApiToken :: Text  -- ^ Habitica API token (x-api-key header)
  }
  deriving stock (Eq, Show)

-- | Default config pointing to production Habitica.
-- Credentials must be filled in.
defaultHabiticaConfig :: HabiticaConfig
defaultHabiticaConfig = HabiticaConfig
  { hcBaseUrl  = "https://habitica.com/api/v3"
  , hcUserId   = ""
  , hcApiToken = ""
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with HTTP manager.
data HabiticaEnv = HabiticaEnv
  { heConfig  :: HabiticaConfig
  , heManager :: Manager
  }

-- | Create a new environment (creates TLS manager).
mkHabiticaEnv :: HabiticaConfig -> IO HabiticaEnv
mkHabiticaEnv config = do
  manager <- newManager tlsManagerSettings
  pure HabiticaEnv { heConfig = config, heManager = manager }


-- ════════════════════════════════════════════════════════════════════════════
-- HABITICA API RESPONSE WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Habitica wraps all responses in {"success": bool, "data": ..., "error"?: ...}
data HabiticaResponse a
  = HabiticaSuccess a
  | HabiticaFailure Text  -- error message
  deriving stock (Show)

instance FromJSON a => FromJSON (HabiticaResponse a) where
  parseJSON = withObject "HabiticaResponse" $ \v -> do
    success <- v .: "success"
    if success
      then HabiticaSuccess <$> v .: "data"
      else HabiticaFailure <$> v .: "message"


-- ════════════════════════════════════════════════════════════════════════════
-- HTTP HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Make a GET request to Habitica API.
habiticaGet
  :: FromJSON a
  => HabiticaEnv
  -> String        -- ^ Path (e.g., "/user")
  -> IO (Either HabiticaError a)
habiticaGet env path = habiticaRequest env "GET" path Nothing

-- | Make a POST request to Habitica API.
habiticaPost
  :: (ToJSON body, FromJSON a)
  => HabiticaEnv
  -> String        -- ^ Path
  -> body          -- ^ Request body
  -> IO (Either HabiticaError a)
habiticaPost env path body = habiticaRequest env "POST" path (Just $ encode body)

-- | Make an HTTP request to Habitica API.
habiticaRequest
  :: FromJSON a
  => HabiticaEnv
  -> String                  -- ^ Method
  -> String                  -- ^ Path
  -> Maybe LBS.ByteString    -- ^ Request body
  -> IO (Either HabiticaError a)
habiticaRequest env method path maybeBody = do
  let config = heConfig env
      url = T.unpack (hcBaseUrl config) <> path

  result <- try @SomeException $ do
    req0 <- parseRequest url
    let req = req0
          { method = TE.encodeUtf8 (T.pack method)
          , requestHeaders =
              [ ("x-api-user", TE.encodeUtf8 $ hcUserId config)
              , ("x-api-key", TE.encodeUtf8 $ hcApiToken config)
              , ("Content-Type", "application/json")
              , ("x-client", "tidepool-habitica-executor/0.1")
              ]
          , requestBody = case maybeBody of
              Nothing -> RequestBodyLBS ""
              Just b  -> RequestBodyLBS b
          }
    httpLbs req (heManager env)

  case result of
    Left exc -> pure $ Left $ HabiticaOther $ "HTTP error: " <> T.pack (show exc)
    Right resp -> parseResponse resp

-- | Parse Habitica API response.
parseResponse :: FromJSON a => Response LBS.ByteString -> IO (Either HabiticaError a)
parseResponse resp = do
  let status = statusCode (responseStatus resp)
      body = responseBody resp

  case status of
    401 -> pure $ Left HabiticaUnauthorized
    429 -> pure $ Left HabiticaRateLimited
    404 -> pure $ Left $ HabiticaNotFound "Resource not found"
    _ | status >= 200 && status < 300 ->
        case eitherDecode body of
          Left err -> pure $ Left $ HabiticaOther $ "JSON decode error: " <> T.pack err
          Right (HabiticaSuccess a) -> pure $ Right a
          Right (HabiticaFailure msg) -> pure $ Left $ parseErrorMessage msg
      | otherwise ->
        -- Try to parse error message from response body
        case eitherDecode @(HabiticaResponse Value) body of
          Right (HabiticaFailure msg) -> pure $ Left $ parseErrorMessage msg
          _ -> pure $ Left $ HabiticaOther $ "HTTP " <> T.pack (show status)

-- | Parse Habitica error messages into structured errors.
parseErrorMessage :: Text -> HabiticaError
parseErrorMessage msg
  | "session" `T.isInfixOf` T.toLower msg = HabiticaSessionExpired
  | "rate" `T.isInfixOf` T.toLower msg = HabiticaRateLimited
  | "not found" `T.isInfixOf` T.toLower msg = HabiticaNotFound msg
  | "unauthorized" `T.isInfixOf` T.toLower msg = HabiticaUnauthorized
  | otherwise = HabiticaOther msg


-- ════════════════════════════════════════════════════════════════════════════
-- API OPERATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | GET /user
getUser :: HabiticaEnv -> IO (Either HabiticaError UserInfo)
getUser env = habiticaGet env "/user"

-- | GET /tasks/user?type=<type>
getTasks :: HabiticaEnv -> TaskType -> IO (Either HabiticaError [HabiticaTask])
getTasks env taskType =
  habiticaGet env $ "/tasks/user?type=" <> taskTypeStr taskType

-- | GET /tasks/user?type=todos (with checklist expansion)
-- Habitica returns todos with their checklists included.
fetchTodos :: HabiticaEnv -> IO (Either HabiticaError [FetchedTodo])
fetchTodos env = habiticaGet env "/tasks/user?type=todos"

-- | POST /tasks/{taskId}/score/{direction}
scoreTask :: HabiticaEnv -> TaskId -> Direction -> IO (Either HabiticaError ScoreResult)
scoreTask env (TaskId tid) dir =
  habiticaPost env ("/tasks/" <> T.unpack tid <> "/score/" <> directionStr dir) emptyObject

-- | POST /tasks/user (create todo)
createTodo :: HabiticaEnv -> Text -> IO (Either HabiticaError TodoId)
createTodo env title = do
  result <- habiticaPost env "/tasks/user" $ object
    [ "type" .= ("todo" :: Text)
    , "text" .= title
    ]
  pure $ case result of
    Left err -> Left err
    Right (val :: Value) -> case parseCreatedTaskId val of
      Nothing -> Left $ HabiticaOther "Failed to parse created task ID"
      Just tid -> Right tid

-- | POST /tasks/{taskId}/checklist
addChecklistItem :: HabiticaEnv -> TodoId -> Text -> IO (Either HabiticaError ChecklistItemId)
addChecklistItem env (TodoId tid) itemText = do
  result <- habiticaPost env ("/tasks/" <> T.unpack tid <> "/checklist") $ object
    [ "text" .= itemText
    ]
  pure $ case result of
    Left err -> Left err
    Right (val :: Value) -> case parseChecklistItemId val of
      Nothing -> Left $ HabiticaOther "Failed to parse checklist item ID"
      Just cid -> Right cid


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

taskTypeStr :: TaskType -> String
taskTypeStr Habits  = "habits"
taskTypeStr Dailys  = "dailys"
taskTypeStr Todos   = "todos"
taskTypeStr Rewards = "rewards"

directionStr :: Direction -> String
directionStr Up   = "up"
directionStr Down = "down"

emptyObject :: Value
emptyObject = object []

-- | Parse task ID from created task response.
-- Habitica returns the full task object; we extract _id.
parseCreatedTaskId :: Value -> Maybe TodoId
parseCreatedTaskId val =
  TodoId <$> parseMaybe (withObject "Task" (.: "_id")) val

-- | Parse checklist item ID from response.
-- Response format: {"id": "...", "text": "...", ...}
parseChecklistItemId :: Value -> Maybe ChecklistItemId
parseChecklistItemId val =
  ChecklistItemId <$> parseMaybe (withObject "Checklist" (.: "id")) val

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Habitica effects using native HTTP client.
--
-- This interpreter makes real API calls to Habitica using http-client-tls.
-- It handles both the crashing variants (FetchTodos, etc.) and the Try variants
-- (FetchTodosTry, etc.).
--
-- = Example
--
-- @
-- import Control.Monad.Freer (runM)
--
-- main = do
--   let config = HabiticaConfig {...}
--   env <- mkHabiticaEnv config
--   runM $ runHabitica env $ do
--     todos <- fetchTodos
--     forM_ todos $ \\todo -> logInfo (ftTitle todo)
-- @
runHabitica :: LastMember IO effs => HabiticaEnv -> Eff (Habitica ': effs) a -> Eff effs a
runHabitica env = interpret $ \case
  -- Crashing variants (error on failure)
  FetchTodos -> sendM $ do
    result <- fetchTodos env
    case result of
      Left err -> error $ "Habitica.fetchTodos: " <> show err
      Right todos -> pure todos

  AddChecklistItem tid item -> sendM $ do
    result <- addChecklistItem env tid item
    case result of
      Left err -> error $ "Habitica.addChecklistItem: " <> show err
      Right cid -> pure cid

  CreateTodo title -> sendM $ do
    result <- createTodo env title
    case result of
      Left err -> error $ "Habitica.createTodo: " <> show err
      Right tid -> pure tid

  GetUser -> sendM $ do
    result <- getUser env
    case result of
      Left err -> error $ "Habitica.getUser: " <> show err
      Right info -> pure info

  ScoreTask tid dir -> sendM $ do
    result <- scoreTask env tid dir
    case result of
      Left err -> error $ "Habitica.scoreTask: " <> show err
      Right sr -> pure sr

  GetTasks tt -> sendM $ do
    result <- getTasks env tt
    case result of
      Left err -> error $ "Habitica.getTasks: " <> show err
      Right tasks -> pure tasks

  -- Try variants (return Either)
  FetchTodosTry -> sendM $ fetchTodos env

  AddChecklistItemTry tid item -> sendM $ addChecklistItem env tid item

  CreateTodoTry title -> sendM $ createTodo env title

  GetUserTry -> sendM $ getUser env

  ScoreTaskTry tid dir -> sendM $ scoreTask env tid dir

  GetTasksTry tt -> sendM $ getTasks env tt
