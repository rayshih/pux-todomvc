module Main where

import Prelude hiding (div)
import App.Events (AppEffects)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Pux (CoreEffects, App, start, EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onClick, onChange, targetValue)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, input, button)
import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.Markup (text, (!), (#!))
import Data.Foldable (for_)
import Data.Array (snoc, filter)

data Event = FieldChanged DOMEvent
           | AddEntry DOMEvent
           | DeleteEntry Int DOMEvent

newtype Entry = Entry { id :: Int
                      , title :: String
                      }

mkEntry :: Int -> String -> Entry
mkEntry id title = Entry { id, title }

newtype State = State { nextId :: Int
                      , editingField :: String
                      , entries :: Array Entry
                      }

init :: String -> State
init url = State { nextId: 0
                 , editingField: ""
                 , entries: []
                 }

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (FieldChanged ev) (State s) =
  noEffects $ State s { editingField = targetValue ev }

foldp (AddEntry ev) (State s) =
  noEffects $ State s { nextId = s.nextId + 1
                      , editingField = ""
                      , entries = snoc s.entries $ mkEntry s.nextId s.editingField
                      }

foldp (DeleteEntry id ev) (State s) =
  noEffects $ State s { entries = filter (\(Entry en) -> en.id /= id) s.entries }

renderEntry :: Entry -> HTML Event
renderEntry (Entry { id, title }) = div do
  text title
  button #! onClick (DeleteEntry id) $ text "x"

view :: State -> HTML Event
view (State st) = div do
  div $ text $ "nextId = " <> show st.nextId
  div $ text $ "editingField = " <> show st.editingField
  div do
    for_ st.entries renderEntry
  div do
    input #! onChange FieldChanged ! value st.editingField
    button #! onClick AddEntry $ text "Add"

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
