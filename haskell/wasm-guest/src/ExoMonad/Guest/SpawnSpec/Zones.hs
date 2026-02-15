{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.SpawnSpec.Zones
  ( zoneDefaults,
    mergeZoneDefaults,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import ExoMonad.Guest.SpawnSpec.Types

-- | Get defaults for a zone.
zoneDefaults :: Zone -> ZoneDefaults
zoneDefaults Proto =
  ZoneDefaults
    { zdReadFirst = Set.fromList ["proto/CLAUDE.md", "proto/effects/"],
      zdVerify =
        Set.fromList
          [ Command "nix develop -c just proto-gen",
            Command "cargo check --workspace"
          ],
      zdEnvVars = Map.singleton "PROTOC" "/nix/store/xpcghksv92lyd463hzpkq8lp0v6pz8qq-protobuf-32.1/bin/protoc",
      zdRules = ["Both proto copies (proto/ and rust/exomonad-proto/proto/) must be byte-identical"]
    }
zoneDefaults RustCore =
  ZoneDefaults
    { zdReadFirst = Set.fromList ["rust/CLAUDE.md", "rust/exomonad-core/"],
      zdVerify =
        Set.fromList
          [ Command "cargo check --workspace",
            Command "cargo test --workspace",
            Command "cargo clippy --workspace -- -D warnings"
          ],
      zdEnvVars = Map.empty,
      zdRules = ["Check ALL prost struct construction sites when adding proto fields"]
    }
zoneDefaults HaskellWasm =
  ZoneDefaults
    { zdReadFirst = Set.fromList ["haskell/CLAUDE.md", "haskell/wasm-guest/"],
      zdVerify =
        Set.fromList
          [ Command "cabal build wasm-guest",
            Command "just wasm-all"
          ],
      zdEnvVars = Map.empty,
      zdRules = ["All MCP tools defined in Haskell WASM, never in Rust"]
    }
zoneDefaults RustBinary =
  ZoneDefaults
    { zdReadFirst = Set.fromList ["rust/exomonad/CLAUDE.md"],
      zdVerify = Set.fromList [Command "cargo check -p exomonad", Command "cargo test -p exomonad"],
      zdEnvVars = Map.empty,
      zdRules = []
    }
zoneDefaults Justfile =
  ZoneDefaults
    { zdReadFirst = Set.fromList ["justfile"],
      zdVerify = Set.fromList [Command "just --list"],
      zdEnvVars = Map.empty,
      zdRules = []
    }

-- | Merge defaults for a set of zones.
mergeZoneDefaults :: Set.Set Zone -> ZoneDefaults
mergeZoneDefaults = foldMap zoneDefaults
