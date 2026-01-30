-- | Typed LLM invocation DSL.
--
-- This module provides a typed, standalone API for making LLM calls from
-- within effectful code (e.g., tool handlers). Unlike the graph machinery,
-- this is designed for simple, one-shot LLM invocations.
--
-- = Design
--
-- Two separate concerns:
--
-- 1. __Template rendering__ - @render template ctx -> Text@ (reusable for GH issues, PRs, etc.)
-- 2. __LLM invocation__ - @call cfg (System text) (User text)@
--
-- = Usage
--
-- @
-- import ExoMonad.LLM
--
-- analyzeDoc
--   :: (Member LLMCall es, Member Log es)
--   => Document
--   -> Eff es (Either CallError Report)
-- analyzeDoc doc = do
--   -- Render templates
--   let sysText = render systemTpl SystemCtx { role = "analyst" }
--   let userText = render userTpl UserCtx { document = doc.content }
--
--   -- Build config with output type
--   let cfg = defaultLLM \@Report
--         & model Sonnet
--         & maxTokens 4096
--
--   -- Make the call
--   callNoTools cfg (System sysText) (User userText)
-- @
--
-- = With Tools
--
-- @
-- let cfg = defaultLLM \@Report
--       & model Opus
--       & tools MyTools
--           { search = \\args -> searchIn doc args
--           , readSection = \\args -> pure (doc.sections !! args.index)
--           }
--
-- call cfg (System sysText) (User userText)
-- @
--
-- = Running (No Tools)
--
-- @
-- import ExoMonad.LLM.Interpret (runLLMCall)
-- import ExoMonad.LLM.Interpreter (runLLMComplete, mkLLMEnv)
--
-- main = do
--   env <- mkLLMEnv (LLMSocketConfig ".exomonad/sockets/service.sock")
--   runM
--     $ runLLMComplete env
--     $ runLLMCall
--     $ yourCode
-- @
--
-- = Running (With Tools)
--
-- @
-- import ExoMonad.LLM.Interpret (runLLMCallWithTools)
--
-- let myTools = MyTools { search = searchHandler, lookup = lookupHandler }
-- runM
--   $ runLLMComplete env
--   $ runLLMCallWithTools myTools
--   $ yourCode
-- @
module ExoMonad.LLM
  ( -- * LLM Call Effect
    LLMCall,

    -- * Making Calls
    call,
    callNoTools,

    -- * Interpreters
    runLLMCall,
    runLLMCallWithTools,

    -- * Configuration
    CallConfig,
    defaultLLM,
    Model (..),
    NoTools,

    -- * Builder Functions
    model,
    maxTokens,
    tools,

    -- * Prompt Types
    System (..),
    User (..),

    -- * Error Types
    CallError (..),

    -- * Tool Support
    ToolRecord (..),
    ToolSchema (..),
    dispatchHandler,
    ToolDispatchError (..),

    -- * TH Derivation
    deriveToolRecord,
    Tool (..),

    -- * Template Rendering

    -- | For rendering Jinja templates to Text. See "ExoMonad.Template.Render"
    -- for more details.
    render,
    renderText,
    TypedTemplate,
    typedTemplateFile,
    GingerContext,
  )
where

-- Core types

-- Builder pattern
import ExoMonad.LLM.Builder
  ( defaultLLM,
    maxTokens,
    model,
    tools,
  )
-- Tool support

-- TH derivation for ToolRecord

-- Effect and main functions
import ExoMonad.LLM.Effect
  ( LLMCall,
    call,
    callNoTools,
  )
-- Interpreters
import ExoMonad.LLM.Interpret
  ( runLLMCall,
    runLLMCallWithTools,
  )
import ExoMonad.LLM.Tools
  ( ToolDispatchError (..),
    ToolRecord (..),
    ToolSchema (..),
    dispatchHandler,
  )
import ExoMonad.LLM.Tools.TH
  ( Tool (..),
    deriveToolRecord,
  )
import ExoMonad.LLM.Types
  ( CallConfig,
    CallError (..),
    Model (..),
    NoTools,
    System (..),
    User (..),
  )
-- Template rendering
import ExoMonad.Template.Render
  ( GingerContext,
    TypedTemplate,
    render,
    renderText,
    typedTemplateFile,
  )
