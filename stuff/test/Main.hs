module Main (main) where

import Control.Monad

main :: IO ()
main = do
  txt <- readFile "test/foo.txt"
  guard $ txt == "bar\n"
