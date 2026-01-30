{-# LANGUAGE OverloadedStrings #-}

-- | Executable to generate the TypeScript npm package.
--
-- Usage:
--
-- > cabal run generate-ts-package -- ./output
--
-- This generates a complete npm package in ./output containing:
--
-- * Generated TypeScript files (graphs.ts, exports.ts, dispatcher.ts, index.ts)
-- * Static files copied from exomonad-generated-ts/static/
-- * Package configuration (package.json, tsconfig.json)
--
-- The output can be consumed by deploy/ or any other TypeScript project:
--
-- > # In deploy/package.json
-- > "dependencies": {
-- >   "exomonad-generated-ts": "file:../exomonad-wasm/output"
-- > }
module Main where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import ExoMonad.Generated.Codegen
  ( generateDispatcherTs,
    generateExportsTs,
    generateGraphsTs,
    generateHandlersTs,
    generateIndexTs,
    generatePackageJson,
    generateRoutingTs,
    generateTsConfig,
  )
import ExoMonad.Generated.GraphSpecs (allEffectSpecs)
import ExoMonad.Wasm.Registry (registryGraphSpecs)
import ExoMonad.Wasm.Registry.Default (setupDefaultRegistry)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- ============================================================================
-- Package Configuration
-- ============================================================================

packageName :: Text
packageName = "exomonad-generated-ts"

packageVersion :: Text
packageVersion = "0.1.0"

-- ============================================================================
-- Static Files
-- ============================================================================

-- | Static TypeScript files to copy from exomonad-generated-ts/static/
staticFiles :: [String]
staticFiles =
  [ "protocol.ts",
    "loader.ts",
    "jsffi.ts"
  ]

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputDir] -> generatePackage outputDir
    _ -> do
      hPutStrLn stderr "Usage: generate-ts-package <output-dir>"
      hPutStrLn stderr ""
      hPutStrLn stderr "Example:"
      hPutStrLn stderr "  cabal run generate-ts-package -- ./output"
      hPutStrLn stderr ""
      hPutStrLn stderr "This generates a complete npm package with:"
      hPutStrLn stderr "  - Generated TypeScript types for graph registry"
      hPutStrLn stderr "  - Type-safe WASM export dispatcher"
      hPutStrLn stderr "  - Protocol types and WASM loader"

-- | Generate the complete TypeScript package.
generatePackage :: FilePath -> IO ()
generatePackage outputDir = do
  putStrLn $ "Generating TypeScript package in: " ++ outputDir

  -- Setup registry first
  setupDefaultRegistry

  -- Create output directories
  createDirectoryIfMissing True (outputDir </> "src")
  createDirectoryIfMissing True (outputDir </> "wasm")

  -- Generate TypeScript files
  -- Graph specs come from the unified Registry (single source of truth)
  graphSpecs <- registryGraphSpecs

  putStrLn "  Generating graphs.ts..."
  TIO.writeFile (outputDir </> "src" </> "graphs.ts") (generateGraphsTs graphSpecs)

  putStrLn "  Generating exports.ts..."
  TIO.writeFile (outputDir </> "src" </> "exports.ts") (generateExportsTs graphSpecs)

  putStrLn "  Generating dispatcher.ts..."
  TIO.writeFile (outputDir </> "src" </> "dispatcher.ts") (generateDispatcherTs graphSpecs)

  putStrLn "  Generating routing.ts..."
  TIO.writeFile (outputDir </> "src" </> "routing.ts") (generateRoutingTs allEffectSpecs)

  putStrLn "  Generating handlers.generated.ts..."
  TIO.writeFile (outputDir </> "src" </> "handlers.generated.ts") (generateHandlersTs allEffectSpecs)

  putStrLn "  Generating index.ts..."
  TIO.writeFile (outputDir </> "src" </> "index.ts") generateIndexTs

  -- Generate package configuration
  putStrLn "  Generating package.json..."
  TIO.writeFile (outputDir </> "package.json") (generatePackageJson packageName packageVersion)

  putStrLn "  Generating tsconfig.json..."
  TIO.writeFile (outputDir </> "tsconfig.json") generateTsConfig

  -- Copy static files
  copyStaticFiles outputDir

  putStrLn ""
  putStrLn "Package generated successfully!"
  putStrLn ""
  putStrLn "Next steps:"
  putStrLn $ "  1. cd " ++ outputDir
  putStrLn "  2. npm install"
  putStrLn "  3. npm run build"
  putStrLn ""
  putStrLn "To use in deploy/:"
  putStrLn "  Add to package.json: \"exomonad-generated-ts\": \"file:../path/to/output\""

-- | Copy static TypeScript files from exomonad-generated-ts/static/
copyStaticFiles :: FilePath -> IO ()
copyStaticFiles outputDir = do
  -- Try to find static files relative to current directory or common locations
  let staticDirs =
        [ "exomonad-generated-ts/static",
          "../exomonad-generated-ts/static",
          "static" -- In case running from exomonad-generated-ts
        ]

  forM_ staticFiles $ \fileName -> do
    let srcPaths = map (</> fileName) staticDirs
    found <- findExisting srcPaths

    case found of
      Just srcPath -> do
        putStrLn $ "  Copying " ++ fileName ++ "..."
        copyFile srcPath (outputDir </> "src" </> fileName)
      Nothing -> do
        hPutStrLn stderr $ "  WARNING: Could not find static file: " ++ fileName
        hPutStrLn stderr $ "  Looked in: " ++ show staticDirs

-- | Find the first existing file from a list of paths.
findExisting :: [FilePath] -> IO (Maybe FilePath)
findExisting [] = pure Nothing
findExisting (p : ps) = do
  exists <- doesFileExist p
  if exists
    then pure (Just p)
    else findExisting ps
