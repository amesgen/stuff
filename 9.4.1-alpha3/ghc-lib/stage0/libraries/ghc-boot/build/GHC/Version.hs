module GHC.Version where

import Prelude -- See Note [Why do we import Prelude here?]

cProjectGitCommitId   :: String
cProjectGitCommitId   = "a44bdc2720015c03d57f470b759ece7fab29a57a"

cProjectVersion       :: String
cProjectVersion       = "9.4.0.20220623"

cProjectVersionInt    :: String
cProjectVersionInt    = "904"

cProjectPatchLevel    :: String
cProjectPatchLevel    = "020220623"

cProjectPatchLevel1   :: String
cProjectPatchLevel1   = "0"

cProjectPatchLevel2   :: String
cProjectPatchLevel2   = "20220623"
