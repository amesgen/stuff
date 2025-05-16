{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Language.Haskell.TH.Syntax as TH

main :: IO ()
main = putStrLn $(TH.lift =<< TH.runIO (readFile "app/foo.txt"))
