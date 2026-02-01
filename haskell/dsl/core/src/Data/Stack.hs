{-# LANGUAGE DeriveFunctor #-}

-- | LIFO (Last-In-First-Out) Stack Data Structure
--
-- A stack supports these core operations:
--   * 'push': Add an element to the top
--   * 'pop': Remove and return the top element
--   * 'peek': View the top element without removing
--   * 'isEmpty': Check if the stack is empty
--
-- The implementation maintains strict LIFO ordering: the most recently
-- pushed element is always the first to be popped.
--
-- = Examples
--
-- >>> let s0 = empty
-- >>> let s1 = push 1 s0
-- >>> let s2 = push 2 s1
-- >>> let s3 = push 3 s2
-- >>> pop s3
-- Just (3, s2)
-- >>> pop (pop s3)
-- Just (2, s1)
-- >>> isEmpty s0
-- True
-- >>> isEmpty s3
-- False
module Data.Stack
  ( -- * Stack Type
    Stack,

    -- * Constructor
    empty,

    -- * Core Operations
    push,
    pop,
    peek,
    isEmpty,
  )
where

import Prelude hiding (empty)

-- | A stack is a collection that maintains LIFO (Last-In-First-Out) ordering.
--
-- The stack can be empty or contain a list of elements, with the most
-- recently added element at the head for fast access.
data Stack a
  = Empty
  | Cons a (Stack a)
  deriving (Functor, Show, Eq)

-- | Create an empty stack.
--
-- === Law
-- > isEmpty empty == True
-- > pop empty == Nothing
--
-- === Time Complexity
-- O(1)
empty :: Stack a
empty = undefined

-- | Push an element onto the top of the stack.
--
-- The pushed element becomes the first element to be popped.
-- Stack size increases by one.
--
-- === Law
-- > pop (push x s) == Just (x, s)
--
-- === Time Complexity
-- O(1)
push :: a -> Stack a -> Stack a
push = undefined

-- | Remove and return the top element of the stack.
--
-- Returns 'Nothing' if the stack is empty.
-- Returns 'Just (x, s')' where x is the top element and s' is the
-- remaining stack.
--
-- === Laws
-- > pop empty == Nothing
-- > pop (push x s) == Just (x, s)
--
-- === Time Complexity
-- O(1)
pop :: Stack a -> Maybe (a, Stack a)
pop = undefined

-- | View the top element without removing it.
--
-- Returns 'Nothing' if the stack is empty.
-- The stack is not modified.
--
-- === Laws
-- > peek empty == Nothing
-- > peek (push x s) == Just x
-- > peek s == fst <$> pop s  (ignores the rest)
--
-- === Time Complexity
-- O(1)
peek :: Stack a -> Maybe a
peek = undefined

-- | Check if the stack is empty.
--
-- === Laws
-- > isEmpty empty == True
-- > isEmpty (push x s) == False
--
-- === Time Complexity
-- O(1)
isEmpty :: Stack a -> Bool
isEmpty = undefined
