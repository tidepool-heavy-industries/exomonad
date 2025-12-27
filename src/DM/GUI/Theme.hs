-- | Dark noir theme for the DM Agent
--
-- Blades in the Dark aesthetic: deep charcoal, muted gold accents.
module DM.GUI.Theme
  ( -- * Theme
    noirTheme
    -- * Tier colors for dice
  , TierColor(..)
  , tierColor
  , tierColorCSS
  ) where

import Data.Text (Text)

import Tidepool.GUI.Theme (Theme(..), ColorPalette(..))

-- | The dark noir theme for Blades in the Dark
noirTheme :: Theme
noirTheme = Theme
  { themeName = "noir"
  , themeColors = noirPalette
  , themeFontMain = "'Crimson Text', Georgia, serif"
  , themeFontMono = "'JetBrains Mono', 'Fira Code', monospace"
  }

-- | Noir color palette
noirPalette :: ColorPalette
noirPalette = ColorPalette
  { cpBackground    = "#1a1a1e"   -- Deep charcoal
  , cpBackgroundAlt = "#252529"   -- Slightly lighter
  , cpBackgroundHover = "#2f2f35" -- Card backgrounds / hover
  , cpAccent        = "#c9a227"   -- Muted gold
  , cpAccentDim     = "#8b7420"   -- Dimmer gold
  , cpText          = "#e8e8e8"   -- Off-white
  , cpTextMuted     = "#a0a0a5"   -- Muted text
  , cpTextDim       = "#666670"   -- Very muted
  , cpSuccess       = "#4a7c4a"   -- Muted green
  , cpWarning       = "#8b7420"   -- Amber/gold
  , cpDanger        = "#7c4a4a"   -- Muted red
  , cpCritical      = "#c9a227"   -- Gold for critical success
  }

-- | Outcome tier color mapping
data TierColor
  = TierCritical
  | TierSuccess
  | TierPartial
  | TierBad
  | TierDisaster
  deriving (Show, Eq, Ord)

-- | Get the color for a tier
tierColor :: TierColor -> Text
tierColor TierCritical = "#c9a227"  -- Gold
tierColor TierSuccess  = "#4a7c4a"  -- Muted green
tierColor TierPartial  = "#8b7420"  -- Amber
tierColor TierBad      = "#7c4a4a"  -- Muted red
tierColor TierDisaster = "#4a0000"  -- Deep red

-- | Generate CSS class for a tier
tierColorCSS :: TierColor -> Text
tierColorCSS tier = case tier of
  TierCritical -> "tier-critical"
  TierSuccess  -> "tier-success"
  TierPartial  -> "tier-partial"
  TierBad      -> "tier-bad"
  TierDisaster -> "tier-disaster"
