module Main where

import App.Events (AppEffects)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (filter, snoc)
import Data.Foldable (for_)
import Pux (CoreEffects, App, start, EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, button, input)
import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

data Event = FieldChanged DOMEvent
           | AddEntry DOMEvent
           | DeleteEntry Int DOMEvent
           | EditEntry Int DOMEvent
           | ChangeEntry Int DOMEvent
           | UpdateEntry Int DOMEvent
           | CancelEditEntry Int DOMEvent

newtype Entry = Entry { id :: Int
                      , description :: String
                      , editingDesc :: String
                      , isEditing :: Boolean
                      }

mkEntry :: Int -> String -> Entry
mkEntry id description = Entry { id
                               , description
                               , editingDesc: description
                               , isEditing: false
                               }

newtype State = State { nextId :: Int
                      , editingField :: String
                      , entries :: Array Entry
                      }

init :: String -> State
init url = State { nextId: 0
                 , editingField: ""
                 , entries: []
                 }

updateEntryWithId :: (Entry -> Entry) -> Int -> Array Entry -> Array Entry
updateEntryWithId f id = map applyIfMatch
  where applyIfMatch entry@(Entry en) | en.id == id = f entry
                                      | otherwise = entry

setEditState :: Boolean -> Entry -> Entry
setEditState isEditing (Entry en) = Entry $ en { isEditing = isEditing }

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

foldp (EditEntry id ev) (State s) =
  noEffects $ State s {
    entries = updateEntryWithId (setEditState true) id s.entries
  }

foldp (ChangeEntry id ev) (State s) =
  noEffects $ State s {
    entries = updateEntryWithId update id s.entries
  }
  where
    update (Entry en) = Entry $ en { editingDesc = targetValue ev }

foldp (CancelEditEntry id ev) (State s) =
  noEffects $ State s {
    entries = updateEntryWithId update id s.entries
  }

  where
    resetEditingDesc (Entry en) = Entry $ en { editingDesc = en.description }
    update = resetEditingDesc >>> setEditState false

foldp (UpdateEntry id ev) (State s) =
  noEffects $ State s {
    entries = updateEntryWithId update id s.entries
  }

  where
    commitEditingDesc (Entry en) = Entry $ en { description = en.editingDesc }
    update = commitEditingDesc >>> setEditState false

viewEntry :: Entry -> HTML Event
viewEntry (Entry { id, description, editingDesc, isEditing }) = div do
  if isEditing
    then do
      input #! onChange (ChangeEntry id) ! value editingDesc
      button #! onClick (UpdateEntry id) $ text "Update"
      button #! onClick (CancelEditEntry id) $ text "Cancel"
    else do
      text description
      button #! onClick (EditEntry id) $ text "Edit"
      button #! onClick (DeleteEntry id) $ text "x"

view :: State -> HTML Event
view (State st) = div do
  div $ text $ "nextId = " <> show st.nextId
  div $ text $ "editingField = " <> show st.editingField
  div do
    for_ st.entries $ viewEntry
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
