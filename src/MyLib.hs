module MyLib (someFunc) where

import UnliftIO.Resource

someFunc :: ResourceT IO ()
someFunc = pure ()
