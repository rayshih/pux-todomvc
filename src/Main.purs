module Main where

import App.Events (AppEffects)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.KeyboardEvent (code, eventToKeyboardEvent)
import Data.Array (filter, null, snoc, length)
import Data.Either (Either(..))
import Data.Foldable (all, for_)
import Data.Generic (class Generic, gShow)
import Data.Lens ((%~), (.~))
import Data.Lens.Lens (lens)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.String (joinWith)
import Data.String (null) as S
import Data.Symbol (SProxy(..))
import Pux (CoreEffects, App, start, EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onBlur, onChange, onClick, onDoubleClick, onKeyDown, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (focused)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (a, button, div, footer, h1, header, input, label, li, p, section, span, strong, ul)
import Text.Smolder.HTML.Attributes (checked, className, href, placeholder, type', value)
import Text.Smolder.Markup (EventHandlers, text, (!), (!?), (#!))
import Prelude hiding (div)

-- define types and data
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
           | CheckAll Boolean DOMEvent
           | DeleteCompeleted DOMEvent
           | Noop DOMEvent

type EntryRec = { id :: Int
                , description :: String
                , editingDesc :: String
                , isEditing :: Boolean
                , isCompleted :: Boolean
                }
newtype Entry = Entry EntryRec

mkEntry :: Int -> String -> Entry
mkEntry id description = Entry { id
                               , description
                               , editingDesc: description
                               , isEditing: false
                               , isCompleted: false
                               }

type StateRec = { nextId :: Int
                , editingField :: String
                , entries :: Array Entry
                , visibility :: Visibility
                }
newtype State = State StateRec

-- lenses

_Entry :: Lens' Entry EntryRec
_Entry = lens (\(Entry en) -> en) (\_ en -> Entry en)

_isCompleted :: forall r. Lens' { isCompleted :: Boolean | r } Boolean
_isCompleted = prop (SProxy :: SProxy "isCompleted")

_State :: Lens' State StateRec
_State = lens (\(State s) -> s) (\_ s -> State s)

_editingField :: Lens' StateRec String
_editingField = prop (SProxy :: SProxy "editingField")

_entries :: Lens' StateRec (Array Entry)
_entries = prop (SProxy :: SProxy "entries")

-- add show to easier debug
derive instance genericVisibility :: Generic Visibility
derive instance genericEntry :: Generic Entry
derive instance genericState :: Generic State

instance showVis :: Show Visibility where
  show = gShow

instance showEntry :: Show Entry where
  show = gShow

instance showState :: Show State where
  show = gShow

-- init state
init :: String -> State
init url = State { nextId: 0
                 , editingField: ""
                 , entries: []
                 , visibility: All
                 }

-- update reusable util functions
-- update only if match id
updateEntryWithId :: (Entry -> Entry) -> Int -> Array Entry -> Array Entry
updateEntryWithId f id = map applyIfMatch
  where applyIfMatch entry@(Entry en) | en.id == id = f entry
                                      | otherwise = entry

setEditState :: Boolean -> Entry -> Entry
setEditState isEditing (Entry en) = Entry $ en { isEditing = isEditing }

-- the foldp
foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (FieldChanged ev) ss =
  noEffects $ ss # _State <<< _editingField .~ targetValue ev

foldp (AddEntry ev) ss@(State s) =
  if S.null s.editingField
  then noEffects ss
  else noEffects $ State s { nextId = s.nextId + 1
                           , editingField = ""
                           , entries = snoc s.entries $ mkEntry s.nextId s.editingField
                           }

foldp (DeleteEntry id ev) ss =
  noEffects $ ss # _State <<< _entries %~ filter (\(Entry en) -> en.id /= id)

foldp (EditEntry id ev) ss =
  noEffects $ ss # _State <<< _entries %~ map update

  where
    update ee@(Entry en) =
      if en.id == id
      then Entry en { isEditing = true }
      else Entry en { isEditing = false }

foldp (ChangeEntry id ev) ss =
  noEffects $ ss # _State <<< _entries %~ updateEntryWithId update id
  where
    update (Entry en) = Entry $ en { editingDesc = targetValue ev }

foldp (CancelEditEntry id ev) ss =
  noEffects $ ss # _State <<< _entries %~ updateEntryWithId update id
  where
    resetEditingDesc (Entry en) = Entry $ en { editingDesc = en.description }
    update = resetEditingDesc >>> setEditState false

foldp (UpdateEntry id ev) ss =
  noEffects $ ss # _State <<< _entries %~ updateEntryWithId update id
  where
    commitEditingDesc (Entry en) = Entry $ en { description = en.editingDesc }
    update = commitEditingDesc >>> setEditState false

foldp (ToggleComplete id ev) ss =
  noEffects $ ss # _State <<< _entries %~ updateEntryWithId update id
  where
    update = _Entry <<< _isCompleted %~ not

foldp (ChangeVisibility vis ev) (State s) =
  noEffects $ State s { visibility = vis }

foldp (CheckAll complete ev) ss =
  noEffects $ ss # _State <<< _entries %~ map update
  where
    update = _Entry <<< _isCompleted .~ complete

foldp (DeleteCompeleted ev) (State s) =
  noEffects $ State s { entries = filter (\(Entry en) -> not en.isCompleted) s.entries }

foldp (Noop ev) ss = noEffects ss

-- keyboard event handlers
filterKey :: String -> (DOMEvent -> Event) -> (DOMEvent -> Event)
filterKey keyCode f ev =
  case runExcept $ code <$> eventToKeyboardEvent ev of
    (Right code) | code == keyCode -> f ev
    _ -> Noop ev

onEnter :: (DOMEvent -> Event) -> EventHandlers (DOMEvent -> Event)
onEnter f = onKeyDown (filterKey "Enter" f)

-- render functions
viewEntry :: Entry -> HTML Event
viewEntry (Entry { id, description, editingDesc, isEditing, isCompleted }) =
  li ! className cName $ do
    div ! className "view" $ do
      (input
        ! className "toggle"
        ! type' "checkbox"
        !? isCompleted) (checked $ show isCompleted)
        #! onClick (ToggleComplete id)
      label
        #! onDoubleClick (EditEntry id)
        $ text description
      button
        ! className "destroy"
        #! onClick (DeleteEntry id)
        $ text ""
    (input
      ! className "edit"
      ! value editingDesc
      !? isEditing) focused
      #! onChange (ChangeEntry id)
      #! onEnter (UpdateEntry id)
      #! onBlur (CancelEditEntry id)

  where
    cEditing = if isEditing then "editing" else ""
    cCompleted = if isCompleted then "completed" else ""
    cName = joinWith " " <<< filter (not <<< S.null) $ [cEditing, cCompleted]

view :: State -> HTML Event
view (State st) = div do
  section ! className "todoapp" $ do
    inputForNewEntry
    viewEntries
    controls
  infoFooter

  where
    -- data
    completedEntries = filter completed st.entries
    leftEntries = filter (not <<< completed) st.entries
    visibleEntries = filter matchVis st.entries
    allCompleted = all completed st.entries
    hasEntries = not <<< null $ st.entries

    completed (Entry { isCompleted }) = isCompleted

    matchVis (Entry { isCompleted }) =
      if st.visibility == All
      then true
      else if st.visibility == Active
           then not isCompleted
           else isCompleted

    -- sub view functions
    inputForNewEntry =
      div do
        header ! className "header" $ do
          h1 $ text "todos"
        input
          ! className "new-todo"
          ! placeholder "What needs to be done?"
          ! value st.editingField
          #! onChange FieldChanged
          #! onEnter AddEntry

    viewEntries =
      when hasEntries do
        section ! className "main" $ do
          (input
            ! className "toggle-all"
            ! type' "checkbox"
            !? allCompleted) (checked "true")
            #! onClick (CheckAll $ not allCompleted)
        ul ! className "todo-list" $ do
          for_ visibleEntries $ viewEntry

    controls = when hasEntries do
      footer ! className "footer" $ do
        span ! className "todo-count" $ do
          strong $ text $ show $ length leftEntries
          text " item left"

        ul ! className "filters" $ do
          visBtn All "All" st.visibility
          visBtn Active "Active" st.visibility
          visBtn Completed "Completed" st.visibility

        when (not <<< null $ completedEntries) do
          button
            ! className "clear-completed"
            #! onClick DeleteCompeleted
            $ text ("Clear completed (" <> (show $ length completedEntries) <> ")")

    visBtn :: Visibility -> String -> Visibility -> HTML Event
    visBtn vis label currentVis = li do
      (a
        !? isCurrent) (className "selected")
        #! onClick (ChangeVisibility vis) $ text label
      where
        isCurrent = vis == currentVis

infoFooter :: HTML Event
infoFooter = footer ! className "info" $ do
  p $ text "Double-click to edit a todo"
  p do
    text "Template by "
    a ! href "http://sindresorhus.com" $ text "Sindre Sorhus"
  p do
    text "Created by "
    a $ text "Ray Shih"
  p do
    text "Part of "
    a ! href "http://todomvc.com" $ text "TodoMVC"

--------------------
-- infrastructure --
--------------------

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  log "Do Reload"
  log $ show state

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
