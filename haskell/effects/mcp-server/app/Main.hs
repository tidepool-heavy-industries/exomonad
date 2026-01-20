-- | Example MCP server
--
-- Demonstrates exposing a simple echo tool via MCP.
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Tidepool.Schema (HasJSONSchema(..), TidepoolDefault(..))
import Tidepool.StructuredOutput ()
import Tidepool.StructuredOutput.Instances ()
import Tidepool.MCP.Server
import Tidepool.MCP.Tools.BdDispatch (bdDispatchTool)

-- | Example input type
newtype EchoInput = EchoInput { message :: Text }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Example output type
newtype EchoOutput = EchoOutput { echoed :: Text }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Simple echo handler
echoHandler :: EchoInput -> IO EchoOutput
echoHandler (EchoInput msg) = pure $ EchoOutput ("Echo: " <> msg)

main :: IO ()
main = do
  let tool = makeMcpTool
        (Proxy @(TidepoolDefault EchoInput))
        "echo"
        "Echo back the input message"
        (\(TidepoolDefault input) -> echoHandler input)

  let config = McpConfig
        { mcName = "tidepool-mcp-server"
        , mcVersion = "0.1.0"
        , mcTools = [tool, bdDispatchTool]
        }

  runMcpServer config
