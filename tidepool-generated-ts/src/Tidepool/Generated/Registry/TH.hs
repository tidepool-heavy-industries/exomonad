{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell splice for generating FFI exports.
--
-- This module provides the 'deriveFFIExports' splice which generates
-- all the boilerplate FFI code for exposing graphs via WASM.
--
-- = Usage
--
-- In tidepool-wasm/Ffi.hs:
--
-- @
-- $(deriveFFIExports
--     [ GraphFFISpec "test" "TestGraph" 'computeHandlerWasm 'testGraphState 'testExitToOutput "compute"
--     , GraphFFISpec "example" "ExampleGraph" 'runExampleGraph 'exampleGraphState 'exampleExitToOutput "classify"
--     ])
-- @
--
-- = Generated Code
--
-- For each graph, this generates:
--
-- 1. @initialize_X@ / @initializeImpl_X@ - Start graph execution
-- 2. @step_X@ / @stepImpl_X@ - Continue execution with effect result
-- 3. @getGraphInfo_X@ / @getGraphInfoImpl_X@ - Get graph metadata
-- 4. @getGraphState_X@ / @getGraphStateImpl_X@ - Get runtime state
-- 5. @foreign export javascript@ declarations (WASM only)
module Tidepool.Generated.Registry.TH
  ( -- * Configuration
    GraphFFISpec(..)

    -- * TH Splices
  , deriveFFIExports
  , deriveGraphFFI
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (mkName)


-- | Configuration for generating FFI exports for a single graph.
--
-- Note: We use explicit accessor functions since the codebase uses NoFieldSelectors.
data GraphFFISpec = GraphFFISpec
  { _gfsId :: String
    -- ^ Graph ID used in export names (e.g., "test" -> initialize_test)
  , _gfsGraphName :: String
    -- ^ Human-readable graph name for error messages (e.g., "TestGraph")
  , _gfsRunner :: Name
    -- ^ Name of the runner function (e.g., 'computeHandlerWasm)
  , _gfsStateRef :: Name
    -- ^ Name of the global IORef WasmState (e.g., 'testGraphState)
  , _gfsToOutput :: Name
    -- ^ Name of the result-to-StepOutput converter (e.g., 'testExitToOutput)
  , _gfsNodeName :: String
    -- ^ Node name for phase tracking (e.g., "compute")
  , _gfsNodes :: [String]
    -- ^ List of node names in the graph (e.g., ["entry", "compute", "exit"])
  , _gfsEdges :: [(String, String)]
    -- ^ List of edges as (from, to) pairs (e.g., [("entry", "compute"), ("compute", "exit")])
  }

-- Explicit accessor functions (pattern matching since NoFieldSelectors is on)
gfsId :: GraphFFISpec -> String
gfsId (GraphFFISpec x _ _ _ _ _ _ _) = x

gfsGraphName :: GraphFFISpec -> String
gfsGraphName (GraphFFISpec _ x _ _ _ _ _ _) = x

gfsRunner :: GraphFFISpec -> Name
gfsRunner (GraphFFISpec _ _ x _ _ _ _ _) = x

gfsStateRef :: GraphFFISpec -> Name
gfsStateRef (GraphFFISpec _ _ _ x _ _ _ _) = x

gfsToOutput :: GraphFFISpec -> Name
gfsToOutput (GraphFFISpec _ _ _ _ x _ _ _) = x

gfsNodeName :: GraphFFISpec -> String
gfsNodeName (GraphFFISpec _ _ _ _ _ x _ _) = x

gfsNodes :: GraphFFISpec -> [String]
gfsNodes (GraphFFISpec _ _ _ _ _ _ x _) = x

gfsEdges :: GraphFFISpec -> [(String, String)]
gfsEdges (GraphFFISpec _ _ _ _ _ _ _ x) = x


-- | Generate FFI exports for all graphs in the list.
--
-- @
-- $(deriveFFIExports specs)
-- @
--
-- This generates all the FFI machinery: initialize, step, getGraphInfo,
-- getGraphState functions and their foreign export declarations.
deriveFFIExports :: [GraphFFISpec] -> Q [Dec]
deriveFFIExports specs = concat <$> mapM deriveGraphFFI specs


-- | Generate FFI exports for a single graph.
--
-- Generates:
-- - @wasmResultToOutput_X@ - Convert WasmResult to StepOutput
-- - @initializeImpl_X@ - Implementation taking Text
-- - @initialize_X@ - Platform-specific wrapper (JSString for WASM, Text for native)
-- - @stepImpl_X@ - Implementation taking Text
-- - @step_X@ - Platform-specific wrapper
-- - @getGraphInfoImpl_X@ - Implementation returning Text
-- - @getGraphInfo_X@ - Platform-specific wrapper
-- - @getGraphStateImpl_X@ - Implementation returning Text
-- - @getGraphState_X@ - Platform-specific wrapper
-- - Foreign export declarations (WASM only, via CPP)
deriveGraphFFI :: GraphFFISpec -> Q [Dec]
deriveGraphFFI spec = do
  -- Generate all the declarations
  wasmResultDecs <- genWasmResultToOutput spec
  initImplDec <- genInitializeImpl spec
  initDec <- genInitialize spec
  stepImplDec <- genStepImpl spec
  stepDec <- genStep spec
  infoImplDec <- genGetGraphInfoImpl spec
  infoDec <- genGetGraphInfo spec
  stateImplDec <- genGetGraphStateImpl spec
  stateDec <- genGetGraphState spec

  pure $ concat
    [ wasmResultDecs
    , initImplDec
    , initDec
    , stepImplDec
    , stepDec
    , infoImplDec
    , infoDec
    , stateImplDec
    , stateDec
    ]


-- ============================================================================
-- Code Generators
-- ============================================================================

-- | Generate wasmResultToOutput_X function.
--
-- Note: We don't generate a type signature here - we let GHC infer the type
-- from the toOutput function usage. This avoids issues with polymorphic types.
--
-- @
-- wasmResultToOutput_test (WasmComplete a) = do
--   writeIORef testGraphState Idle
--   pure $ testExitToOutput a
-- wasmResultToOutput_test (WasmYield eff resume) = do
--   let phase = PhaseInNode "compute"
--   writeIORef testGraphState (Waiting (SomeCont resume testExitToOutput) phase)
--   pure $ StepYield eff (GraphState phase [])
-- wasmResultToOutput_test (WasmError msg) = do
--   writeIORef testGraphState Idle
--   pure $ mkErrorOutput msg
-- @
genWasmResultToOutput :: GraphFFISpec -> Q [Dec]
genWasmResultToOutput spec = do
  let funName = mkName $ "wasmResultToOutput_" ++ gfsId spec
      stateRef = gfsStateRef spec
      toOutput = gfsToOutput spec
      nodeName = gfsNodeName spec

  -- Pattern variables
  let aVar = mkName "a"
      effVar = mkName "eff"
      resumeVar = mkName "resume"
      msgVar = mkName "msg"
      phaseVar = mkName "phase"

  -- Clause 1: WasmComplete a
  let completeClause = Clause
        [ConP (mkName "WasmComplete") [] [VarP aVar]]
        (NormalB $ DoE Nothing
          [ NoBindS $ AppE (AppE (VarE (mkName "writeIORef")) (VarE stateRef)) (ConE (mkName "Idle"))
          , NoBindS $ AppE (VarE (mkName "pure")) (AppE (VarE toOutput) (VarE aVar))
          ])
        []

  -- Clause 2: WasmYield eff resume
  let yieldClause = Clause
        [ConP (mkName "WasmYield") [] [VarP effVar, VarP resumeVar]]
        (NormalB $ DoE Nothing
          [ LetS [ValD (VarP phaseVar) (NormalB $ AppE (ConE (mkName "PhaseInNode")) (LitE (StringL nodeName))) []]
          , NoBindS $ AppE (AppE (VarE (mkName "writeIORef")) (VarE stateRef))
              (AppE (AppE (ConE (mkName "Waiting"))
                (AppE (AppE (ConE (mkName "SomeCont")) (VarE resumeVar)) (VarE toOutput)))
                (VarE phaseVar))
          , NoBindS $ AppE (VarE (mkName "pure"))
              (AppE (AppE (ConE (mkName "StepYield")) (VarE effVar))
                (AppE (AppE (ConE (mkName "GraphState")) (VarE phaseVar)) (ListE [])))
          ])
        []

  -- Clause 3: WasmError msg
  let errorClause = Clause
        [ConP (mkName "WasmError") [] [VarP msgVar]]
        (NormalB $ DoE Nothing
          [ NoBindS $ AppE (AppE (VarE (mkName "writeIORef")) (VarE stateRef)) (ConE (mkName "Idle"))
          , NoBindS $ AppE (VarE (mkName "pure")) (AppE (VarE (mkName "mkErrorOutput")) (VarE msgVar))
          ])
        []

  let funDec = FunD funName [completeClause, yieldClause, errorClause]

  -- No type signature - let GHC infer from toOutput usage
  pure [funDec]


-- | Generate initializeImpl_X function.
--
-- @
-- initializeImpl_test :: Text -> IO Text
-- initializeImpl_test inputJson =
--   case eitherDecodeStrict (encodeUtf8 inputJson) of
--     Left err -> pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
--     Right entry -> do
--       let result = initializeWasm (computeHandlerWasm entry)
--       output <- wasmResultToOutput_test result
--       pure $ encodeStepOutput output
-- @
genInitializeImpl :: GraphFFISpec -> Q [Dec]
genInitializeImpl spec = do
  let funName = mkName $ "initializeImpl_" ++ gfsId spec
      runner = gfsRunner spec
      wasmResultFn = mkName $ "wasmResultToOutput_" ++ gfsId spec

  -- Type signature: Text -> IO Text
  let typeSig = SigD funName
        (AppT (AppT ArrowT (ConT (mkName "Text")))
              (AppT (ConT (mkName "IO")) (ConT (mkName "Text"))))

  -- Variables
  let inputJsonVar = mkName "inputJson"
      errVar = mkName "err"
      entryVar = mkName "entry"
      resultVar = mkName "result"
      outputVar = mkName "output"

  -- Left clause body
  let leftBody = AppE (VarE (mkName "pure"))
        (AppE (VarE (mkName "encodeStepOutput"))
          (AppE (VarE (mkName "mkErrorOutput"))
            (InfixE
              (Just (LitE (StringL "JSON parse error: ")))
              (VarE (mkName "<>"))
              (Just (AppE (VarE (mkName "T.pack")) (VarE errVar))))))

  -- Right clause body
  let rightBody = DoE Nothing
        [ LetS [ValD (VarP resultVar) (NormalB
            (AppE (VarE (mkName "initializeWasm"))
              (AppE (VarE runner) (VarE entryVar)))) []]
        , BindS (VarP outputVar) (AppE (VarE wasmResultFn) (VarE resultVar))
        , NoBindS $ AppE (VarE (mkName "pure"))
            (AppE (VarE (mkName "encodeStepOutput")) (VarE outputVar))
        ]

  -- Main function body
  let body = CaseE
        (AppE (VarE (mkName "eitherDecodeStrict"))
          (AppE (VarE (mkName "encodeUtf8")) (VarE inputJsonVar)))
        [ Match (ConP (mkName "Left") [] [VarP errVar]) (NormalB leftBody) []
        , Match (ConP (mkName "Right") [] [VarP entryVar]) (NormalB rightBody) []
        ]

  let funDec = FunD funName [Clause [VarP inputJsonVar] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate initialize_X function (platform-specific wrapper).
--
-- For WASM: takes JSString, calls initializeImpl_X, returns JSString
-- For native: just calls initializeImpl_X directly
genInitialize :: GraphFFISpec -> Q [Dec]
genInitialize spec = do
  let funName = mkName $ "initialize_" ++ gfsId spec
      implName = mkName $ "initializeImpl_" ++ gfsId spec
      inputVar = mkName "input"

  -- Native version: Text -> IO Text
  let nativeTypeSig = SigD funName
        (AppT (AppT ArrowT (ConT (mkName "Text")))
              (AppT (ConT (mkName "IO")) (ConT (mkName "Text"))))

  let nativeBody = AppE (VarE implName) (VarE inputVar)
  let nativeDec = FunD funName [Clause [VarP inputVar] (NormalB nativeBody) []]

  -- For now, just generate native version
  -- WASM wrapper and foreign export will be handled by CPP in Ffi.hs
  pure [nativeTypeSig, nativeDec]


-- | Generate stepImpl_X function.
--
-- Uses wasmResultToOutputGeneric which takes the stateRef as a parameter
-- so it can write to the correct per-graph state.
genStepImpl :: GraphFFISpec -> Q [Dec]
genStepImpl spec = do
  let funName = mkName $ "stepImpl_" ++ gfsId spec
      stateRef = gfsStateRef spec
      graphName = gfsGraphName spec
      nodeName = gfsNodeName spec

  -- Type signature: Text -> IO Text
  let typeSig = SigD funName
        (AppT (AppT ArrowT (ConT (mkName "Text")))
              (AppT (ConT (mkName "IO")) (ConT (mkName "Text"))))

  -- Variables
  let resultJsonVar = mkName "resultJson"
      mStateVar = mkName "mState"
      resumeVar = mkName "resume"
      toOutputVar = mkName "toOutputFn"
      phaseVar = mkName "_phase"
      errVar = mkName "err"
      effectResultVar = mkName "effectResult"
      nextResultVar = mkName "nextResult"
      outputVar = mkName "output"

  -- Error message for not initialized
  let notInitMsg = graphName ++ " not initialized - call initialize_" ++ gfsId spec ++ "() before step_" ++ gfsId spec ++ "()"

  -- Main function body
  let body = DoE Nothing
        [ BindS (VarP mStateVar) (AppE (VarE (mkName "readIORef")) (VarE stateRef))
        , NoBindS $ CaseE (VarE mStateVar)
            [ -- Idle case
              Match (ConP (mkName "Idle") [] [])
                (NormalB $ AppE (VarE (mkName "pure"))
                  (AppE (VarE (mkName "encodeStepOutput"))
                    (AppE (VarE (mkName "mkErrorOutput"))
                      (LitE (StringL notInitMsg)))))
                []
            , -- Waiting case
              Match (ConP (mkName "Waiting") []
                      [ ConP (mkName "SomeCont") [] [VarP resumeVar, VarP toOutputVar]
                      , VarP phaseVar
                      ])
                (NormalB $ CaseE
                  (AppE (VarE (mkName "eitherDecodeStrict"))
                    (AppE (VarE (mkName "encodeUtf8")) (VarE resultJsonVar)))
                  [ -- Parse error
                    Match (ConP (mkName "Left") [] [VarP errVar])
                      (NormalB $ AppE (VarE (mkName "pure"))
                        (AppE (VarE (mkName "encodeStepOutput"))
                          (AppE (VarE (mkName "mkErrorOutput"))
                            (InfixE
                              (Just (LitE (StringL "JSON parse error: ")))
                              (VarE (mkName "<>"))
                              (Just (AppE (VarE (mkName "T.pack")) (VarE errVar)))))))
                      []
                  , -- Successful parse
                    Match (ConP (mkName "Right") [] [VarP effectResultVar])
                      (NormalB $ DoE Nothing
                        [ LetS [ValD (VarP nextResultVar)
                            (NormalB (AppE (VarE resumeVar) (VarE effectResultVar)))
                            []]
                        , BindS (VarP outputVar)
                            -- wasmResultToOutputGeneric stateRef nodeName nextResult toOutputFn
                            (foldl AppE (VarE (mkName "wasmResultToOutputGeneric"))
                              [ VarE stateRef
                              , LitE (StringL nodeName)
                              , VarE nextResultVar
                              , VarE toOutputVar
                              ])
                        , NoBindS $ AppE (VarE (mkName "pure"))
                            (AppE (VarE (mkName "encodeStepOutput")) (VarE outputVar))
                        ])
                      []
                  ])
                []
            ]
        ]

  let funDec = FunD funName [Clause [VarP resultJsonVar] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate step_X function (platform-specific wrapper).
genStep :: GraphFFISpec -> Q [Dec]
genStep spec = do
  let funName = mkName $ "step_" ++ gfsId spec
      implName = mkName $ "stepImpl_" ++ gfsId spec
      resultVar = mkName "result"

  let typeSig = SigD funName
        (AppT (AppT ArrowT (ConT (mkName "Text")))
              (AppT (ConT (mkName "IO")) (ConT (mkName "Text"))))

  let body = AppE (VarE implName) (VarE resultVar)
  let funDec = FunD funName [Clause [VarP resultVar] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate getGraphInfoImpl_X function.
--
-- Generates JSON with:
-- - name: Graph name
-- - id: Graph ID
-- - nodes: Array of node names (strings)
-- - edges: Array of {from, to} objects
genGetGraphInfoImpl :: GraphFFISpec -> Q [Dec]
genGetGraphInfoImpl spec = do
  let funName = mkName $ "getGraphInfoImpl_" ++ gfsId spec
      graphName = gfsGraphName spec
      nodes = gfsNodes spec
      edges = gfsEdges spec

  -- Type signature: IO Text
  let typeSig = SigD funName (AppT (ConT (mkName "IO")) (ConT (mkName "Text")))

  -- Build nodes array: ["entry", "compute", "exit"] (plain strings)
  let nodeStrings = ListE $ map (\n ->
        SigE (LitE (StringL n)) (ConT (mkName "Text"))
        ) nodes

  -- Build edges array: [object ["from" .= "entry", "to" .= "compute"], ...]
  let edgeObjects = ListE $ map (\(from, to) ->
        AppE (VarE (mkName "object"))
          (ListE
            [ InfixE (Just (LitE (StringL "from")))
                (VarE (mkName ".="))
                (Just (SigE (LitE (StringL from)) (ConT (mkName "Text"))))
            , InfixE (Just (LitE (StringL "to")))
                (VarE (mkName ".="))
                (Just (SigE (LitE (StringL to)) (ConT (mkName "Text"))))
            ])
        ) edges

  -- Body: pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $ object [...]
  let body = AppE (VarE (mkName "pure"))
        (AppE (VarE (mkName "TL.toStrict"))
          (AppE (VarE (mkName "TLE.decodeUtf8"))
            (AppE (VarE (mkName "encode"))
              (AppE (VarE (mkName "object"))
                (ListE
                  [ InfixE (Just (LitE (StringL "name")))
                      (VarE (mkName ".="))
                      (Just (SigE (LitE (StringL graphName)) (ConT (mkName "Text"))))
                  , InfixE (Just (LitE (StringL "id")))
                      (VarE (mkName ".="))
                      (Just (SigE (LitE (StringL (gfsId spec))) (ConT (mkName "Text"))))
                  , InfixE (Just (LitE (StringL "nodes")))
                      (VarE (mkName ".="))
                      (Just nodeStrings)
                  , InfixE (Just (LitE (StringL "edges")))
                      (VarE (mkName ".="))
                      (Just edgeObjects)
                  ])))))

  let funDec = FunD funName [Clause [] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate getGraphInfo_X function (platform-specific wrapper).
genGetGraphInfo :: GraphFFISpec -> Q [Dec]
genGetGraphInfo spec = do
  let funName = mkName $ "getGraphInfo_" ++ gfsId spec
      implName = mkName $ "getGraphInfoImpl_" ++ gfsId spec

  let typeSig = SigD funName (AppT (ConT (mkName "IO")) (ConT (mkName "Text")))
  let body = VarE implName
  let funDec = FunD funName [Clause [] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate getGraphStateImpl_X function.
genGetGraphStateImpl :: GraphFFISpec -> Q [Dec]
genGetGraphStateImpl spec = do
  let funName = mkName $ "getGraphStateImpl_" ++ gfsId spec
      stateRef = gfsStateRef spec

  -- Type signature: IO Text
  let typeSig = SigD funName (AppT (ConT (mkName "IO")) (ConT (mkName "Text")))

  let mStateVar = mkName "mState"
      graphStateVar = mkName "graphState"
      phaseVar = mkName "phase"

  -- Body
  let body = DoE Nothing
        [ BindS (VarP mStateVar) (AppE (VarE (mkName "readIORef")) (VarE stateRef))
        , LetS [ValD (VarP graphStateVar)
            (NormalB $ CaseE (VarE mStateVar)
              [ Match (ConP (mkName "Idle") [] [])
                  (NormalB $ AppE (AppE (ConE (mkName "GraphState"))
                    (ConE (mkName "PhaseIdle")))
                    (ListE []))
                  []
              , Match (ConP (mkName "Waiting") [] [WildP, VarP phaseVar])
                  (NormalB $ AppE (AppE (ConE (mkName "GraphState"))
                    (VarE phaseVar))
                    (ListE []))
                  []
              ])
            []]
        , NoBindS $ AppE (VarE (mkName "pure"))
            (AppE (VarE (mkName "TL.toStrict"))
              (AppE (VarE (mkName "TLE.decodeUtf8"))
                (AppE (VarE (mkName "encode")) (VarE graphStateVar))))
        ]

  let funDec = FunD funName [Clause [] (NormalB body) []]

  pure [typeSig, funDec]


-- | Generate getGraphState_X function (platform-specific wrapper).
genGetGraphState :: GraphFFISpec -> Q [Dec]
genGetGraphState spec = do
  let funName = mkName $ "getGraphState_" ++ gfsId spec
      implName = mkName $ "getGraphStateImpl_" ++ gfsId spec

  let typeSig = SigD funName (AppT (ConT (mkName "IO")) (ConT (mkName "Text")))
  let body = VarE implName
  let funDec = FunD funName [Clause [] (NormalB body) []]

  pure [typeSig, funDec]
