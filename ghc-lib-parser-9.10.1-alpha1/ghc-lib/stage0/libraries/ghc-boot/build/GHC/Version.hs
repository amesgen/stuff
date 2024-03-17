module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "515a34f44d93b2666829d7b004e78dba92060078"

cProjectVersion       :: String
cProjectVersion       = "9.10.0.20240319"

cProjectVersionInt    :: String
cProjectVersionInt    = "910"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020240319"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20240319"
