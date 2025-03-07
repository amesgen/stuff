module Main where

import qualified Stuff
import System.Environment

main :: IO ()
main = do
  [out] <- getArgs
  Stuff.prerenderTo out
