{-# LANGUAGE OverloadedStrings #-}
-- | Theme for the Tidying GUI
--
-- A calm, minimal aesthetic inspired by Marie Kondo's philosophy:
-- soft off-white backgrounds, muted sage green accents, warm gray text.
module Tidying.GUI.Theme
  ( tidyingTheme
  , tidyingPalette
  ) where

import Tidepool.GUI.Theme (Theme(..), ColorPalette(..))

-- | Calming color palette for tidying
--
-- Inspired by Japanese minimalism and natural materials:
-- - Warm off-white like natural paper
-- - Sage green like bamboo leaves
-- - Soft grays like stone
tidyingPalette :: ColorPalette
tidyingPalette = ColorPalette
  { cpBackground    = "#faf9f7"   -- Warm off-white (natural paper)
  , cpBackgroundAlt = "#f2f0ed"   -- Slightly darker for cards
  , cpBackgroundHover = "#e8e5e1" -- Hover state
  , cpAccent        = "#7d9a78"   -- Sage green (bamboo)
  , cpAccentDim     = "#a8bba4"   -- Lighter sage
  , cpText          = "#3d3d3d"   -- Warm charcoal
  , cpTextMuted     = "#6b6b6b"   -- Medium gray
  , cpTextDim       = "#9a9a9a"   -- Light gray
  , cpSuccess       = "#6b9a6b"   -- Soft green
  , cpWarning       = "#b89b5c"   -- Warm amber
  , cpDanger        = "#b06060"   -- Muted coral
  , cpCritical      = "#9a7d7d"   -- Dusty rose
  }

-- | The complete tidying theme
tidyingTheme :: Theme
tidyingTheme = Theme
  { themeName = "tidying"
  , themeColors = tidyingPalette
  , themeFontMain = "'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
  , themeFontMono = "'JetBrains Mono', 'Fira Code', 'SF Mono', monospace"
  }
