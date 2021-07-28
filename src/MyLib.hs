{-# LANGUAGE GADTs #-}

module MyLib  where

data Info a b where
    Cons1 :: a -> Info a b -- ^Cons 1 doc
    Cons2 :: a -> Info a b -- ^Cons 2 doc

data Info' a b where
  Cons1' ::
    a ->
    -- | Cons 1 doc
    Info' a b
  Cons2' ::
    a ->
    -- | Cons 2 doc
    Info' a b
