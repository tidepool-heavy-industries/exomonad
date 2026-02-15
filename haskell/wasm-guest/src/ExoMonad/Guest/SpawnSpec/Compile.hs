{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.SpawnSpec.Compile
  ( compileSpawnSpec,
    compileSpawnSpecPure,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.SpawnSpec.Types
import ExoMonad.Guest.SpawnSpec.Zones (mergeZoneDefaults)

-- | Pure prompt compilation. No IO.
compileSpawnSpecPure :: SpawnSpec -> (Text, CompileReport)
compileSpawnSpecPure spec =
  let zd = mergeZoneDefaults (specZones spec)
      allVerify = zdVerify zd <> specVerify spec
      allReadFirst = zdReadFirst zd
      allRules = zdRules zd
      report =
        CompileReport
          { crZonesApplied = specZones spec,
            crVerifyCommands = allVerify,
            crReadFirstPaths = allReadFirst,
            crRulesApplied = allRules
          }
      prompt =
        T.unlines $
          concat
            [ renderReadFirst allReadFirst,
              renderSteps (specSteps spec),
              renderVerify allVerify (zdEnvVars zd),
              renderDoneCriteria (specDoneCriteria spec),
              renderConstraints (specNoCommit spec) allRules,
              renderContext (specContext spec) (specKeyDecisions spec) (specNonGoals spec)
            ]
   in (prompt, report)

-- | IO wrapper: reads plan file if PlanRef is present.
-- For now, just delegates to pure version. Plan reading will be added later.
compileSpawnSpec :: SpawnSpec -> IO (Text, CompileReport)
compileSpawnSpec = pure . compileSpawnSpecPure

-- Rendering helpers

renderReadFirst :: Set.Set Text -> [Text]
renderReadFirst paths
  | Set.null paths = []
  | otherwise = ["## READ FIRST", ""] <> map ("- " <>) (Set.toAscList paths) <> [""]

renderSteps :: [Step] -> [Text]
renderSteps [] = []
renderSteps steps = ["## STEPS", ""] <> zipWith renderStep [1 ..] steps <> [""]
  where
    renderStep :: Int -> Step -> Text
    renderStep n s =
      T.unwords
        [ T.pack (show n) <> ".",
          stepAction s,
          if null (stepFiles s) then "" else "(" <> T.intercalate ", " (stepFiles s) <> ")"
        ]

renderVerify :: Set.Set Command -> Map.Map Text Text -> [Text]
renderVerify cmds envVars
  | Set.null cmds = []
  | otherwise =
      ["## VERIFY", ""]
        <> (if Map.null envVars then [] else ["Environment:", "```"] <> map (\(k, v) -> "export " <> k <> "=" <> v) (Map.toAscList envVars) <> ["```", ""])
        <> ["```bash"]
        <> map unCommand (Set.toAscList cmds)
        <> ["```", ""]

renderDoneCriteria :: [Text] -> [Text]
renderDoneCriteria [] = []
renderDoneCriteria cs = ["## DONE CRITERIA", ""] <> map ("- " <>) cs <> [""]

renderConstraints :: Bool -> [Text] -> [Text]
renderConstraints noCommit rules =
  let items = (if noCommit then ["Do NOT commit. Send a note when done or if you hit a blocker."] else []) <> rules
   in if null items then [] else ["## CONSTRAINTS", ""] <> map ("- " <>) items <> [""]

renderContext :: Maybe Text -> [Text] -> [Text] -> [Text]
renderContext Nothing [] [] = []
renderContext ctx decisions nonGoals =
  ["## CONTEXT", ""]
    <> maybe [] (\c -> [c, ""]) ctx
    <> (if null decisions then [] else ["**Key decisions:**"] <> map ("- " <>) decisions <> [""])
    <> (if null nonGoals then [] else ["**Non-goals:**"] <> map ("- " <>) nonGoals <> [""])
