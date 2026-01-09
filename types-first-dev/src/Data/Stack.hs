{-# LANGUAGE DeriveGeneric #-}

-- | A LIFO (last-in-first-out) stack data structure.
--
-- This module provides a simple, polymorphic stack with O(1) push and pop operations.
-- The stack is implemented as a single-linked list, providing efficient access to the top element.
module Data.Stack
  ( Stack(..)
    -- * Construction
  , empty
  , push
    -- * Deconstruction
  , pop
  , peek
    -- * Query
  , isEmpty
  ) where

import GHC.Generics (Generic)

-- | A LIFO stack containing elements of type @a@.
--
-- The stack supports O(1) push and pop operations. The constructors
-- are exposed to allow pattern matching and custom operations.
--
-- The kind is @* -> *@, making Stack a unary type constructor
-- that takes a polymorphic element type.
data Stack a
  = -- | An empty stack with no elements.
    Empty
  | -- | A stack with an element at the top and a stack below it.
    --
    -- Invariant: The first element is the top of the stack and will be
    -- the first to be popped.
    Cons a (Stack a)
  deriving (Show, Eq, Generic)

-- | Create an empty stack with no elements.
--
-- This is the base case for stack construction.
empty :: Stack a
empty = Empty

-- | Add an element to the top of the stack.
--
-- Time complexity: O(1)
--
-- The new element becomes the first to be popped. This operation
-- increases the stack size by one.
push :: a -> Stack a -> Stack a
push x s = Cons x s

-- | Remove and return the top element and the remaining stack.
--
-- Time complexity: O(1)
--
-- Returns @Nothing@ if the stack is empty. If successful, returns @Just (top, rest)@
-- where @top@ is the element that was at the top of the stack and @rest@
-- is the stack without that element.
pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (Cons x s) = Just (x, s)

-- | View the top element without removing it from the stack.
--
-- Time complexity: O(1)
--
-- Returns @Nothing@ if the stack is empty, otherwise @Just x@ where @x@
-- is the element at the top of the stack. The stack remains unchanged.
peek :: Stack a -> Maybe a
peek Empty = Nothing
peek (Cons x _) = Just x

-- | Check if the stack is empty.
--
-- Time complexity: O(1)
--
-- Returns @True@ if the stack contains no elements, @False@ otherwise.
isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty (Cons _ _) = False
