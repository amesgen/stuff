module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "4656f1d69bdc88cac62707e57e245348c9e01767"

cProjectVersion       :: String
cProjectVersion       = "9.14.0.20251007"

cProjectVersionInt    :: String
cProjectVersionInt    = "914"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020251007"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20251007"
