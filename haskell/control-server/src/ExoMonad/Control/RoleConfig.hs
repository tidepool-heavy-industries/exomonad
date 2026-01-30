module ExoMonad.Control.RoleConfig
  ( Role (..),
    roleFromText,
    isToolAllowed,
  )
where

import Data.Text (Text)
import ExoMonad.Control.Role.Registry (isToolAllowedTyped)
import ExoMonad.Role (Role (..))

roleFromText :: Text -> Maybe Role
roleFromText "pm" = Just PM
roleFromText "tl" = Just TL
roleFromText "dev" = Just Dev
roleFromText _ = Nothing

isToolAllowed :: Role -> Text -> Bool
isToolAllowed = isToolAllowedTyped
