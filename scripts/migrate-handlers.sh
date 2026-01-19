#!/bin/bash

# Migrate Entry/Exit handlers from Proxy @Type to ()
# This is a mechanical replacement for the Entry/Exit handler boilerplate reduction

set -e

echo "Migrating Entry/Exit handlers from Proxy @Type to ()..."

# Find all Haskell files and replace Proxy patterns
# We target patterns like:
#   = Proxy @SomeType  -- Entry
#   = Proxy @SomeType  -- Exit
# And replace with:
#   = ()  -- Entry
#   = ()  -- Exit

find haskell -name "*.hs" -type f -exec sed -i '' \
  -e 's/= Proxy @[A-Za-z0-9_]*\( *-- Entry\)/= ()\1/g' \
  -e 's/= Proxy @[A-Za-z0-9_]*\( *-- Exit\)/= ()\1/g' {} \;

echo "Migration complete!"
echo ""
echo "Files modified. Run 'cabal build all' to check for any remaining type errors."
