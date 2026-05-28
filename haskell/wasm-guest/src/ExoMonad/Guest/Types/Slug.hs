module ExoMonad.Guest.Types.Slug (Slug, mkSlug, unSlug) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withText, (.=))
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import ExoMonad.Guest.Tool.Schema (JsonSchema (..))

newtype Slug = Slug { unSlug :: Text }
  deriving (Show, Eq)

instance JsonSchema Slug where
  toSchema = object ["type" .= ("string" :: Text)]

-- | Smart constructor. Slugs are `[a-z0-9-]+` with no leading/trailing/double hyphens.
mkSlug :: Text -> Either Text Slug
mkSlug t
  | T.null t = Left "slug cannot be empty"
  | T.any (not . validChar) t = Left "slug must contain only [a-z0-9-]"
  | T.head t == '-' = Left "slug cannot start with hyphen"
  | T.last t == '-' = Left "slug cannot end with hyphen"
  | T.isInfixOf "--" t = Left "slug cannot contain consecutive hyphens"
  | otherwise = Right (Slug t)
  where validChar c = isAsciiLower c || isDigit c || c == '-'

instance FromJSON Slug where
  parseJSON = withText "Slug" $ \t -> case mkSlug t of
    Right s -> pure s
    Left err -> fail (T.unpack err)

instance ToJSON Slug where
  toJSON = toJSON . unSlug
