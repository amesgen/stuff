module B where

ex1 :: String
ex1 = hello <> " " <> foo

ex2 :: [a] -> Int -> a
ex2 l i = l !? i

ex3 = offsetTimeIncrease
