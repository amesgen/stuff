module AD {-# DEPRECATED "dont use this" #-} (hello, foo) where

import A

foo :: String
foo = A.hello
{-# DEPRECATED foo "dont use this" #-}
