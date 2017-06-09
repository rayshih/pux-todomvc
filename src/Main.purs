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
import Text.Smolder.HTML (a, button, div, input, span)
import Text.Smolder.HTML.Attributes (value)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

data Visibility = All | Active | Completed
derive instance eqVis :: Eq Visibility

data Event = FieldChanged DOMEvent
           | AddEntry DOMEvent
           | DeleteEntry Int DOMEvent
           | EditEntry Int DOMEvent
           | ChangeEntry Int DOMEvent
           | UpdateEntry Int DOMEvent
           | CancelEditEntry Int DOMEvent
           | ToggleComplete Int DOMEvent
           | ChangeVisibility Visibility DOMEvent
           | CheckAll DOMEvent
           | DeleteCompeleted DOMEvent

newtype Entry = Entry { id :: Int
                      , description :: String
                      , editingDesc :: String
                      , isEditing :: Boolean
                      , isCompleted :: Boolean
                      }

mkEntry :: Int -> String -> Entry
mkEntry id description = Entry { id
                               , description
                               , editingDesc: description
                               , isEditing: false
                               , isCompleted: false
                               }

newtype State = State { nextId :: Int
                      , editingField :: String
                      , entries :: Array Entry
                      , visibility :: Visibility
                      }

init :: String -> State
init url = State { nextId: 0
                 , editingField: ""
                 , entries: []
                 , visibility: All
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

foldp (CancelEditEntry id ev) ((State s)) =
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

foldp (ToggleComplete id ev) (State s) =
  noEffects $ State s {
    entries = updateEntryWithId update id s.entries
  }

  where
    update (Entry en) = Entry $ en { isCompleted = not en.isCompleted }

foldp (ChangeVisibility vis ev) (State s) =
  noEffects $ State s { visibility = vis }

foldp (CheckAll ev) (State s) =
  noEffects $ State s { entries = map update s.entries}
  where
    update (Entry en) = Entry $ en { isCompleted = true }

foldp (DeleteCompeleted ev) (State s) =
  noEffects $ State s { entries = filter (\(Entry en) -> not en.isCompleted) s.entries }

viewEntry :: Entry -> HTML Event
viewEntry (Entry { id, description, editingDesc, isEditing, isCompleted }) = div do
  checkbox
  rightView
  where
    checkbox = span do
      a #! onClick (ToggleComplete id) $ text $
        "[" <> (if isCompleted then "v" else " ") <> "]"

    rightView = if isEditing
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
    for_ entries' $ viewEntry
  div do
    input #! onChange FieldChanged ! value st.editingField
    button #! onClick AddEntry $ text "Add"
  div do
    text $ "Visibility: "
    visBtn All "All" st.visibility
    visBtn Active "Active" st.visibility
    visBtn Completed "Completed" st.visibility
  div do
    text $ "Control: "
    button #! onClick CheckAll $ text "Check All"
    button #! onClick DeleteCompeleted $ text "Delete Completed"

  where
    entries' = filter matchVis st.entries

    matchVis (Entry { isCompleted }) =
      if st.visibility == All
      then true
      else if st.visibility == Active
           then not isCompleted
           else isCompleted

    visBtn :: Visibility -> String -> Visibility -> HTML Event
    visBtn vis label currentVis = button #! onClick (ChangeVisibility vis) $ text label'
      where label' = if vis == currentVis
                     then label <> "*"
                     else label

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
