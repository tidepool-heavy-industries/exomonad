-- | Server API definition - Servant types for HTTP endpoints.
module Tidepool.Server.API
  ( -- * API Types
    TidepoolAPI
  , StaticAPI
  , api
    -- * Server
  , staticServer
  , tidepoolApp
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status200, status404)
import Network.Wai (responseLBS, pathInfo)
import Servant
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension)

-- | Static file serving API.
type StaticAPI = Raw

-- | Combined API.
type TidepoolAPI = StaticAPI

api :: Proxy TidepoolAPI
api = Proxy

-- | Content type for file extension.
contentTypeFor :: FilePath -> LBS.ByteString
contentTypeFor path = case takeExtension path of
  ".html" -> "text/html; charset=utf-8"
  ".css"  -> "text/css; charset=utf-8"
  ".js"   -> "application/javascript; charset=utf-8"
  ".json" -> "application/json; charset=utf-8"
  ".svg"  -> "image/svg+xml"
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".ico"  -> "image/x-icon"
  ".woff" -> "font/woff"
  ".woff2" -> "font/woff2"
  _       -> "application/octet-stream"

-- | Static file server that serves from a directory.
staticServer :: FilePath -> Server StaticAPI
staticServer staticDir = Tagged $ \req sendResponse -> do
  let pathParts = pathInfo req
      requestedPath = T.unpack $ T.intercalate "/" pathParts
      -- Default to index.html for root
      filePath = if null requestedPath || requestedPath == "/"
                 then staticDir </> "index.html"
                 else staticDir </> requestedPath

  exists <- doesFileExist filePath
  if exists
    then do
      content <- LBS.readFile filePath
      sendResponse $ responseLBS status200
        [("Content-Type", LBS.toStrict $ contentTypeFor filePath)]
        content
    else
      sendResponse $ responseLBS status404
        [("Content-Type", "text/plain")]
        "Not Found"

-- | Create the WAI application.
tidepoolApp :: FilePath -> Application
tidepoolApp staticDir = serve api (staticServer staticDir)
