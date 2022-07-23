{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module MyLib (Foo) where

newtype Foo = Foo (# #)
