{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Role
  ( Role (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)

-- | Agent role for access control and orchestration.
data Role = Dev | TL | PM
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON Role where
  toJSON Dev = String "dev"
  toJSON TL = String "tl"
  toJSON PM = String "pm"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "dev" -> pure Dev
    "tl" -> pure TL
    "pm" -> pure PM
    r -> fail $ "Unknown role: " <> T.unpack r
