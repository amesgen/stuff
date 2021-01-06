{-# LANGUAGE TemplateHaskell #-}

module Lib where

import GitHash

git :: GitInfo
git = $$(tGitInfoCwd)
