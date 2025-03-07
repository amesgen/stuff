{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Stuff (start, prerenderTo) where

import qualified Lucid as L
import Miso

start :: JSM ()
prerenderTo :: FilePath -> IO ()
(start, prerenderTo) = components1
  where
    -- This doesn't work due to the following errors:
    -- > TypeError: parent is undefined
    -- > TypeError: mount is undefined
    components1 =
      (misoComponent $ \_uri -> rootComponent, prerenderApp rootApp)

    -- This also doesn't work (but it doesn't throw errors).
    components2 = (miso $ \_uri -> envelopeApp, prerenderApp envelopeApp)
      where
        envelopeApp = defaultApp () (const noEff) (const $ embed rootComponent) ()

    -- This works well (but it doesn't use components).
    noComponents = (miso $ \_uri -> rootApp, prerenderApp rootApp)

--------------------------------------------------------------------------------

data Model = NotLoaded | Loaded
  deriving (Eq)

initialModel :: Model
initialModel = NotLoaded

data Action = Loading

rootComponent :: Component "root" Model Action
rootComponent = component rootApp

rootApp :: App Model Action
rootApp =
  defaultApp
    initialModel
    (\Loading _model -> noEff Loaded)
    viewModel
    Loading

viewModel :: Model -> View Action
viewModel NotLoaded = div_ [] [text "Loading..."]
viewModel Loaded = div_ [] [text "Loaded"]

--------------------------------------------------------------------------------

-- This is used to write the pre-rendered view to a static HTML file.
prerenderApp :: App model action -> FilePath -> IO ()
prerenderApp app path = L.renderToFile path $ L.doctypehtml_ $ do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
  L.body_ $ do
    L.toHtml $ view app $ model app
    L.script_ [L.src_ "index.js", L.type_ "module"] ("" :: String)
