module Mainish where

import qualified Miso
import qualified Stuff

foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = Miso.run Stuff.start
