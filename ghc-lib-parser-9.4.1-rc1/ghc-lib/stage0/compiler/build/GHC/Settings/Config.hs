module GHC.Settings.Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  ) where

import GHC.Prelude

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "x86_64-unknown-linux"

cHostPlatformString :: String
cHostPlatformString = "x86_64-unknown-linux"

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"

cBooterVersion        :: String
cBooterVersion        = "9.2.3"

cStage                :: String
cStage                = show (1 :: Int)
