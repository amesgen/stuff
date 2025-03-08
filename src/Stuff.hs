{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Stuff (start, prerenderTo) where

import Control.Monad.State.Class
import qualified Lucid as L
import Miso
import Miso.String (ms)

start :: JSM ()
start = misoComponent $ \_uri -> rootComponent

--------------------------------------------------------------------------------

data Model = NotLoaded | Loaded Int
  deriving (Eq)

initialModel :: Model
initialModel = NotLoaded

data Action = Loading | Increment

rootComponent :: Component "root" Model Action
rootComponent =
  component $
    defaultApp
      initialModel
      (fromTransition . updateModel)
      viewModel
      Loading

updateModel :: Action -> Transition Action Model ()
updateModel = \case
  Loading -> put $ Loaded 0
  Increment -> modify $ \case
    NotLoaded -> NotLoaded
    Loaded c -> Loaded $ c + 1

viewModel :: Model -> View Action
viewModel NotLoaded = div_ [] [text "Loading..."]
viewModel (Loaded c) =
  div_
    []
    [ text "Loaded ",
      button_ [onClick Increment] ["+"],
      text $ ms $ " Counter: " <> show c
    ]

--------------------------------------------------------------------------------

-- This is used to write the pre-rendered view to a static HTML file.
prerenderTo :: FilePath -> IO ()
prerenderTo path = L.renderToFile path $ L.doctypehtml_ $ do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
  L.body_ $ do
    L.toHtml rootComponent
    L.script_ [L.src_ "index.js", L.type_ "module"] ("" :: String)
