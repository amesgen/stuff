{-# LANGUAGE TemplateHaskell #-}

module Main where

import Numeric.LinearAlgebra

main :: IO ()
main = print $ inv m
  where
    m :: Matrix Double
    m = (2 >< 2) [1, 2, 3, 4]
