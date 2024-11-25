module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "97b0dff223a6c4cc003adec448104c277f214645"

cProjectVersion       :: String
cProjectVersion       = "9.12.0.20241114"

cProjectVersionInt    :: String
cProjectVersionInt    = "912"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020241114"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20241114"
