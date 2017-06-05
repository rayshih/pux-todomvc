module Main where

import Prelude
import App.Events (AppEffects)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Pux (CoreEffects, App, start, EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.Markup (text)

data Event

newtype State = State {}

init :: String -> State
init url = State {}

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp _ s = noEffects s

view :: State -> HTML Event
view s = text $ "Hello world"

--------------------
-- infrastructure --
--------------------

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
