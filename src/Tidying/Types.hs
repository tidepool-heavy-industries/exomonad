{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core semantic types for the Tidying agent
--
-- These newtypes prevent accidental confusion between different kinds
-- of Text values (items, locations, functions, etc.)
module Tidying.Types
  ( -- * Item identification
    ItemName(..)
  , mkItemName

    -- * Locations
  , Location(..)
  , mkLocation

    -- * Space function
  , SpaceFunction(..)
  , mkSpaceFunction

    -- * Anxiety triggers
  , AnxietyTrigger(..)
  , mkAnxietyTrigger

    -- * Category names
  , CategoryName(..)
  , mkCategoryName
  ) where

import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Name of an item being sorted (e.g., "old magazine", "blue mug")
newtype ItemName = ItemName { unItemName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Smart constructor that strips whitespace
mkItemName :: Text -> ItemName
mkItemName = ItemName . T.strip

-- | Where an item goes (e.g., "desk", "kitchen counter", "closet")
newtype Location = Location { unLocation :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Smart constructor that strips whitespace
mkLocation :: Text -> Location
mkLocation = Location . T.strip

-- | What the space is for (e.g., "workspace", "creative", "bedroom")
newtype SpaceFunction = SpaceFunction { unSpaceFunction :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Smart constructor that strips whitespace
mkSpaceFunction :: Text -> SpaceFunction
mkSpaceFunction = SpaceFunction . T.strip

-- | Topic that causes user anxiety (e.g., "boxes", "papers")
newtype AnxietyTrigger = AnxietyTrigger { unAnxietyTrigger :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Smart constructor that strips whitespace
mkAnxietyTrigger :: Text -> AnxietyTrigger
mkAnxietyTrigger = AnxietyTrigger . T.strip

-- | Name of an emergent category (e.g., "cables", "papers")
newtype CategoryName = CategoryName { unCategoryName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- | Smart constructor that strips whitespace
mkCategoryName :: Text -> CategoryName
mkCategoryName = CategoryName . T.strip
