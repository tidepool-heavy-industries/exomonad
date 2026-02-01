{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Records.RefactorPreview where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.RefactorPreview
  ( DiscardRefactor,
    EnactRefactor,
    RefactorPreview,
    SteerRefactor,
  )
import GHC.Generics (Generic)

data RefactorPreviewTools mode = RefactorPreviewTools
  { refactor_preview :: mode :- RefactorPreview,
    enact_refactor :: mode :- EnactRefactor,
    steer_refactor :: mode :- SteerRefactor,
    discard_refactor :: mode :- DiscardRefactor
  }
  deriving (Generic)

refactorPreviewToolsHandler :: RefactorPreviewTools AsHandler
refactorPreviewToolsHandler =
  RefactorPreviewTools
    { refactor_preview = mkHandler @RefactorPreview,
      enact_refactor = mkHandler @EnactRefactor,
      steer_refactor = mkHandler @SteerRefactor,
      discard_refactor = mkHandler @DiscardRefactor
    }

refactorPreviewToolsSchema :: RefactorPreviewTools AsSchema
refactorPreviewToolsSchema =
  RefactorPreviewTools
    { refactor_preview = mkSchema @RefactorPreview,
      enact_refactor = mkSchema @EnactRefactor,
      steer_refactor = mkSchema @SteerRefactor,
      discard_refactor = mkSchema @DiscardRefactor
    }
