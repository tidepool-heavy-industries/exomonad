#!/usr/bin/env python3
"""
Post-process proto3-suite generated Haskell files:
1. Fix module names: Exomonad -> ExoMonad
2. Strip ToSchema instances (not compatible with WASM)
"""

import re
import sys
from pathlib import Path


def fix_module_names(content: str) -> str:
    """Fix module names from Exomonad to ExoMonad."""
    content = re.sub(r'^module Exomonad\.', 'module ExoMonad.', content, flags=re.MULTILINE)
    content = re.sub(r'import Exomonad\.', 'import ExoMonad.', content)
    content = re.sub(r'qualified Exomonad\.', 'qualified ExoMonad.', content)
    content = re.sub(r'Exomonad\.Ffi\.', 'ExoMonad.Ffi.', content)
    content = re.sub(r'Exomonad\.Common\.', 'ExoMonad.Common.', content)
    content = re.sub(r'Exomonad\.Hook\.', 'ExoMonad.Hook.', content)
    content = re.sub(r'Exomonad\.Agent\.', 'ExoMonad.Agent.', content)
    content = re.sub(r'Exomonad\.Popup\.', 'ExoMonad.Popup.', content)
    return content


def strip_toschema_instances(content: str) -> str:
    """Strip ToSchema instances from proto3-suite generated code."""
    lines = content.split('\n')
    result = []
    skip = False

    for line in lines:
        # Start skipping at ToSchema instance
        if line.startswith('instance (HsJSONPB.ToSchema'):
            skip = True
            continue

        # Stop skipping at next top-level declaration
        if skip:
            # Check for top-level declarations that end a ToSchema block
            if (line.startswith('data ') or
                line.startswith('newtype ') or
                (line.startswith('instance ') and 'ToSchema' not in line)):
                skip = False
                result.append(line)
            # Otherwise skip this line
            continue

        result.append(line)

    return '\n'.join(result)


def process_file(filepath: Path) -> None:
    """Process a single Haskell file."""
    print(f"Processing: {filepath.name}")
    content = filepath.read_text()

    content = fix_module_names(content)
    content = strip_toschema_instances(content)

    filepath.write_text(content)


def main():
    proto_dir = Path(__file__).parent.parent / "haskell" / "proto" / "src" / "ExoMonad"

    for name in ["Ffi.hs", "Common.hs", "Hook.hs", "Agent.hs", "Popup.hs"]:
        filepath = proto_dir / name
        if filepath.exists():
            process_file(filepath)
        else:
            print(f"Warning: {filepath} not found")

    print("Done!")


if __name__ == "__main__":
    main()
