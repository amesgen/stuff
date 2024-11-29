module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "5d938345841e3dff6a1dfa129202ba939a4564c9"

cProjectVersion       :: String
cProjectVersion       = "9.12.0.20241128"

cProjectVersionInt    :: String
cProjectVersionInt    = "912"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020241128"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20241128"
