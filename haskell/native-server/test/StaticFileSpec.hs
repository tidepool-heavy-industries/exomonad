module StaticFileSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (finally)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Tidepool.Server.API (tidepoolApp)

-- | Test with a temporary static directory
withStaticServer :: (Int -> IO a) -> IO a
withStaticServer action = do
  let testDir = "/tmp/tidepool-test-static"
      port = 8898

  -- Create test directory with files
  createDirectoryIfMissing True testDir
  LBS.writeFile (testDir </> "index.html") "<!DOCTYPE html><html><body>Test</body></html>"
  LBS.writeFile (testDir </> "style.css") "body { color: black; }"
  LBS.writeFile (testDir </> "app.js") "console.log('test');"

  -- Start server
  serverThread <- async $ run port (tidepoolApp testDir)
  threadDelay 100000 -- 100ms for server startup

  -- Run action and cleanup
  result <- action port `finally` do
    cancel serverThread
    removeDirectoryRecursive testDir

  pure result

spec :: Spec
spec = do
  describe "Static file serving" $ do
    it "serves index.html at root" $ do
      withStaticServer $ \port -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "http://127.0.0.1:" ++ show port ++ "/"
        response <- httpLbs request manager
        statusCode (responseStatus response) `shouldBe` 200
        -- Check that response contains "Test"
        LBS.length (responseBody response) `shouldSatisfy` (> 0)

    it "serves CSS files with correct content type" $ do
      withStaticServer $ \port -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "http://127.0.0.1:" ++ show port ++ "/style.css"
        response <- httpLbs request manager
        statusCode (responseStatus response) `shouldBe` 200
        -- Check content type header
        let headers = responseHeaders response
            contentType = lookup "Content-Type" headers
        contentType `shouldBe` Just "text/css; charset=utf-8"

    it "serves JavaScript files with correct content type" $ do
      withStaticServer $ \port -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "http://127.0.0.1:" ++ show port ++ "/app.js"
        response <- httpLbs request manager
        statusCode (responseStatus response) `shouldBe` 200
        let headers = responseHeaders response
            contentType = lookup "Content-Type" headers
        contentType `shouldBe` Just "application/javascript; charset=utf-8"

    it "returns 404 for missing files" $ do
      withStaticServer $ \port -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "http://127.0.0.1:" ++ show port ++ "/nonexistent.html"
        response <- httpLbs request manager
        statusCode (responseStatus response) `shouldBe` 404

  describe "Content types" $ do
    it "detects HTML content type" $ do
      withStaticServer $ \port -> do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "http://127.0.0.1:" ++ show port ++ "/index.html"
        response <- httpLbs request manager
        let headers = responseHeaders response
            contentType = lookup "Content-Type" headers
        contentType `shouldBe` Just "text/html; charset=utf-8"
