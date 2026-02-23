grep -rn '#}' --include='*.hs' . | grep -v '#-}' | head -20
