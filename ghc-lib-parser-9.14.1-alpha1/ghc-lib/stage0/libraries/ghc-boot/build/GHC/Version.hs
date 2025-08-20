module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "fca42ecfd273c8db52f43084a9c5e5b50507fa90"

cProjectVersion       :: String
cProjectVersion       = "9.14.0.20250819"

cProjectVersionInt    :: String
cProjectVersionInt    = "914"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020250819"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20250819"
