{-# LANGUAGE ScopedTypeVariables #-}

module Data.StackSpec where

import Test.QuickCheck
import Data.Stack

-- Arbitrary instance for Stack
instance Arbitrary a => Arbitrary (Stack a) where
  arbitrary = sized genStack
    where
      genStack 0 = return Empty
      genStack n = oneof
        [ return Empty
        , Push <$> arbitrary <*> genStack (n - 1)
        ]
  
  shrink Empty = []
  shrink (Push x s) = [s] ++ [Push x' s | x' <- shrink x] ++ [Push x s' | s' <- shrink s]

-- Properties for 'empty'
prop_emptyIsEmpty :: Bool
prop_emptyIsEmpty = isEmpty empty

prop_emptyPeekIsNothing :: Bool
prop_emptyPeekIsNothing = peek empty == (Nothing :: Maybe Int)

prop_emptyPopIsNothing :: Bool
prop_emptyPopIsNothing = pop empty == (Nothing :: Maybe (Int, Stack Int))

-- Properties for 'push'
prop_pushNotEmpty :: Int -> Stack Int -> Bool
prop_pushNotEmpty x s = not (isEmpty (push x s))

prop_pushPeekGivesElement :: Int -> Stack Int -> Bool
prop_pushPeekGivesElement x s = peek (push x s) == Just x

prop_pushPopGivesElementAndOriginalStack :: Int -> Stack Int -> Bool
prop_pushPopGivesElementAndOriginalStack x s = 
  pop (push x s) == Just (x, s)

-- Properties for 'pop'
prop_popEmptyIsNothing :: Bool
prop_popEmptyIsNothing = pop empty == (Nothing :: Maybe (Int, Stack Int))

prop_popPreservesStackStructure :: Stack Int -> Property
prop_popPreservesStackStructure s = not (isEmpty s) ==> 
  case pop s of
    Nothing -> False
    Just (_, s') -> length' s' == length' s - 1
  where
    length' Empty = 0
    length' (Push _ rest) = 1 + length' rest

prop_popThenPushRestores :: Stack Int -> Property
prop_popThenPushRestores s = not (isEmpty s) ==>
  case pop s of
    Nothing -> False
    Just (x, s') -> push x s' == s

-- Properties for 'peek'
prop_peekDoesNotModifyStack :: Stack Int -> Bool
prop_peekDoesNotModifyStack s = 
  let _ = peek s in s == s

prop_peekEmptyIsNothing :: Bool
prop_peekEmptyIsNothing = peek empty == (Nothing :: Maybe Int)

prop_peekMatchesPopElement :: Stack Int -> Property
prop_peekMatchesPopElement s = not (isEmpty s) ==>
  case (peek s, pop s) of
    (Just x, Just (y, _)) -> x == y
    _ -> False

-- Properties for 'isEmpty'
prop_isEmptyOnlyForEmpty :: Stack Int -> Bool
prop_isEmptyOnlyForEmpty s = isEmpty s == (s == Empty)

prop_pushMakesNonEmpty :: Int -> Stack Int -> Bool
prop_pushMakesNonEmpty x s = not (isEmpty (push x s))

-- Algebraic properties
prop_pushPopIdentity :: Int -> Bool
prop_pushPopIdentity x = pop (push x empty) == Just (x, empty)

prop_multiplePushPopOrder :: Int -> Int -> Int -> Bool
prop_multiplePushPopOrder x y z =
  let s = push z (push y (push x empty))
      Just (z', s') = pop s
      Just (y', s'') = pop s'
      Just (x', s''') = pop s''
  in z' == z && y' == y && x' == x && s''' == empty

prop_peekStability :: Stack Int -> Int -> Bool
prop_peekStability s x = peek (push x s) == peek (push x s)

prop_consecutivePushesAndPops :: [Int] -> Bool
prop_consecutivePushesAndPops xs =
  let s = foldl (flip push) empty xs
      popped = unwindStack s
  in popped == reverse xs
  where
    unwindStack Empty = []
    unwindStack s = case pop s of
      Nothing -> []
      Just (x, s') -> x : unwindStack s'

prop_emptyAfterAllPops :: Stack Int -> Bool
prop_emptyAfterAllPops s = popAll s == empty
  where
    popAll Empty = Empty
    popAll stack = case pop stack of
      Nothing -> stack
      Just (_, s') -> popAll s'

-- LIFO property
prop_lifoProperty :: Int -> Int -> Bool
prop_lifoProperty x y =
  let s = push y (push x empty)
  in case pop s of
    Just (top, _) -> top == y  -- Last in (y) is first out
    Nothing -> False

-- Relationship between isEmpty and peek/pop
prop_isEmptyImpliesNoPeek :: Stack Int -> Bool
prop_isEmptyImpliesNoPeek s = 
  if isEmpty s then peek s == Nothing else True

prop_isEmptyImpliesNoPop :: Stack Int -> Bool
prop_isEmptyImpliesNoPop s =
  if isEmpty s then pop s == Nothing else True

-- Size-related properties
prop_pushIncreasesSize :: Int -> Stack Int -> Bool
prop_pushIncreasesSize x s = stackSize (push x s) == stackSize s + 1
  where
    stackSize Empty = 0
    stackSize (Push _ rest) = 1 + stackSize rest

prop_popDecreasesSize :: Stack Int -> Property
prop_popDecreasesSize s = not (isEmpty s) ==>
  case pop s of
    Just (_, s') -> stackSize s' == stackSize s - 1
    Nothing -> False
  where
    stackSize Empty = 0
    stackSize (Push _ rest) = 1 + stackSize rest

-- Main test runner
main :: IO ()
main = do
  putStrLn "Testing 'empty' properties:"
  quickCheck prop_emptyIsEmpty
  quickCheck prop_emptyPeekIsNothing
  quickCheck prop_emptyPopIsNothing
  
  putStrLn "\nTesting 'push' properties:"
  quickCheck prop_pushNotEmpty
  quickCheck prop_pushPeekGivesElement
  quickCheck prop_pushPopGivesElementAndOriginalStack
  quickCheck prop_pushMakesNonEmpty
  quickCheck prop_pushIncreasesSize
  
  putStrLn "\nTesting 'pop' properties:"
  quickCheck prop_popEmptyIsNothing
  quickCheck prop_popPreservesStackStructure
  quickCheck prop_popThenPushRestores
  quickCheck prop_popDecreasesSize
  
  putStrLn "\nTesting 'peek' properties:"
  quickCheck prop_peekDoesNotModifyStack
  quickCheck prop_peekEmptyIsNothing
  quickCheck prop_peekMatchesPopElement
  quickCheck prop_peekStability
  
  putStrLn "\nTesting 'isEmpty' properties:"
  quickCheck prop_isEmptyOnlyForEmpty
  quickCheck prop_isEmptyImpliesNoPeek
  quickCheck prop_isEmptyImpliesNoPop
  
  putStrLn "\nTesting algebraic properties:"
  quickCheck prop_pushPopIdentity
  quickCheck prop_multiplePushPopOrder
  quickCheck prop_consecutivePushesAndPops
  quickCheck prop_emptyAfterAllPops
  quickCheck prop_lifoProperty
  
  putStrLn "\nAll tests completed!"