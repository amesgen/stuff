{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Codec.Compression.Zlib

someFunc :: IO ()
someFunc = putStrLn $ show $ compress "test"
