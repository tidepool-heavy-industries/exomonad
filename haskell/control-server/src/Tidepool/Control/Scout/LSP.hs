{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | LSP integration for semantic exploration.
--
-- Converts LSP results into NodeContext for scoring and training.
module Tidepool.Control.Scout.LSP
  ( -- * NodeContext Construction
    makeNodeContext

    -- * Helpers
  , locationToText
  , formatPosition
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effect.LSP (Location(..), Range(..), Position(..), HoverInfo(..))
import Tidepool.Control.Scout.Types (NodeContext(..))


-- | Create a NodeContext from LSP hover info and location.
--
-- Parameters:
--   - location: The source location (file:line)
--   - hover: Optional hover information from LSP
--   - snippet: Code snippet around the location
--   - depth: Current exploration depth
--   - breadth: Number of siblings at this level
makeNodeContext
  :: Text           -- ^ Location text (e.g., "Edges.hs:89")
  -> Maybe HoverInfo -- ^ Hover info from LSP
  -> Text           -- ^ Code snippet
  -> Int            -- ^ Exploration depth
  -> Int            -- ^ Breadth (sibling count)
  -> NodeContext
makeNodeContext loc hoverInfo snippet depth breadth = NodeContext
  { ncLocation = loc
  , ncHover = fromMaybe "(no hover info)" ((.hoverContents) <$> hoverInfo)
  , ncCodeSnippet = snippet
  , ncDepth = depth
  , ncBreadth = breadth
  }


-- | Convert an LSP Location to a text string.
--
-- Format: "filename:line"
locationToText :: Location -> Text
locationToText loc =
  let uri = loc.locUri
      -- Strip file:// prefix if present
      filename = fromMaybe uri $ T.stripPrefix "file://" uri
      line = loc.locRange.rangeStart.posLine + 1  -- LSP is 0-indexed
  in filename <> ":" <> T.pack (show line)


-- | Format a Position for display (1-indexed).
formatPosition :: Position -> Text
formatPosition pos =
  T.pack (show (pos.posLine + 1)) <> ":" <> T.pack (show (pos.posCharacter + 1))
