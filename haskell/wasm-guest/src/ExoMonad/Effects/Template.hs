{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Template effects for rendering prompts.
module ExoMonad.Effects.Template
  ( TemplateRenderWorkerPrompt,
    renderWorkerPrompt,
  )
where

import Effects.Template qualified as Proto
import ExoMonad.Effect.Class (Effect (..), EffectError, runEffect)

-- | Render worker prompt effect.
data TemplateRenderWorkerPrompt

instance Effect TemplateRenderWorkerPrompt where
  type Input TemplateRenderWorkerPrompt = Proto.RenderWorkerPromptRequest
  type Output TemplateRenderWorkerPrompt = Proto.RenderWorkerPromptResponse
  effectId = "template.render_worker_prompt"

-- | Render a worker prompt using templates.
renderWorkerPrompt :: Proto.RenderWorkerPromptRequest -> IO (Either EffectError Proto.RenderWorkerPromptResponse)
renderWorkerPrompt = runEffect @TemplateRenderWorkerPrompt
