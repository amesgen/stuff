module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "c7f7ba01b8f832c75fb8211837eec16c09991e9c"

cProjectVersion       :: String
cProjectVersion       = "9.4.0.20220430"

cProjectVersionInt    :: String
cProjectVersionInt    = "904"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020220430"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20220430"
