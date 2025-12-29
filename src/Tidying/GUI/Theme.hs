{-# LANGUAGE OverloadedStrings #-}
-- | Theme for the Tidying GUI
--
-- A calm, minimal dark aesthetic inspired by Marie Kondo's philosophy:
-- deep charcoal backgrounds, muted sage green accents, soft light text.
module Tidying.GUI.Theme
  ( tidyingTheme
  , tidyingPalette
  ) where

import Tidepool.GUI.Theme (Theme(..), ColorPalette(..))

-- | Calming dark color palette for tidying
--
-- Inspired by Japanese minimalism - evening zen garden:
-- - Deep charcoal like river stones at dusk
-- - Sage green like moss in shadow
-- - Soft warm whites like paper lanterns
tidyingPalette :: ColorPalette
tidyingPalette = ColorPalette
  { cpBackground    = "#1a1a1a"   -- Deep charcoal
  , cpBackgroundAlt = "#242424"   -- Slightly lighter for cards
  , cpBackgroundHover = "#2e2e2e" -- Hover state
  , cpAccent        = "#8faa8a"   -- Sage green (moss)
  , cpAccentDim     = "#6b8267"   -- Darker sage
  , cpText          = "#e8e6e3"   -- Warm off-white
  , cpTextMuted     = "#a8a5a0"   -- Muted warm gray
  , cpTextDim       = "#6b6966"   -- Dim warm gray
  , cpSuccess       = "#7eb07e"   -- Soft green
  , cpWarning       = "#c4a86c"   -- Warm amber
  , cpDanger        = "#c07070"   -- Muted coral
  , cpCritical      = "#a88a8a"   -- Dusty rose
  }

-- | The complete tidying theme
tidyingTheme :: Theme
tidyingTheme = Theme
  { themeName = "tidying-dark"
  , themeColors = tidyingPalette
  , themeFontMain = "'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
  , themeFontMono = "'JetBrains Mono', 'Fira Code', 'SF Mono', monospace"
  }
