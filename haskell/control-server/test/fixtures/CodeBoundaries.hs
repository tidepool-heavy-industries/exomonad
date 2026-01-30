-- |
-- Test fixture for code extraction boundary detection.
--
-- This file contains various function patterns that test the boundary
-- detection logic in readCodeAtRange.
module Fixtures.CodeBoundaries where

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 1: Function followed by section divider
-- ═══════════════════════════════════════════════════════════════════════════

-- | Simple function that should stop at section divider.
simpleFunction :: Int -> Int
simpleFunction x =
  let y = x + 1
      z = y * 2
   in z + 3

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 2: Function followed by haddock comment
-- ═══════════════════════════════════════════════════════════════════════════

functionBeforeHaddock :: String -> String
functionBeforeHaddock s =
  case s of
    "" -> "empty"
    _ -> "not empty"

-- | This haddock marks the next function.
nextFunction :: Int
nextFunction = 42

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 3: Function followed by double blank lines
-- ═══════════════════════════════════════════════════════════════════════════

functionWithDoubleBlank :: Bool -> Int
functionWithDoubleBlank b =
  if b then 1 else 0

functionAfterDoubleBlank :: Int
functionAfterDoubleBlank = 99

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 4: Function followed by next top-level definition
-- ═══════════════════════════════════════════════════════════════════════════

multiLineFunction :: a -> b -> (a, b)
multiLineFunction a b =
  let pair = (a, b)
   in pair

anotherTopLevel :: Int
anotherTopLevel = 100

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 5: Function with where clause and guards
-- ═══════════════════════════════════════════════════════════════════════════

functionWithWhere :: Int -> Int -> Int
functionWithWhere x y
  | x > y = bigger
  | x < y = smaller
  | otherwise = equal
  where
    bigger = x - y
    smaller = y - x
    equal = 0

-- ═══════════════════════════════════════════════════════════════════════════
-- CASE 6: Long dash comment (should stop)
-- ═══════════════════════════════════════════════════════════════════════════

functionBeforeDashes :: Int
functionBeforeDashes = 1

-------------------------------------------------
-- This is a divider
-------------------------------------------------

functionAfterDashes :: Int
functionAfterDashes = 2
