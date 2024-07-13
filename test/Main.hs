module Main where

import qualified GHC.Parser.Lexer as L

main :: IO ()
main = print "hello"

foo = L.lexer
