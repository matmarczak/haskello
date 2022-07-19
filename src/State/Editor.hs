{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module State.Editor where

import Brick.Main (continue)
import Brick.Types (EventM, Next)
import Brick.Widgets.Edit
  ( Editor
  , editorText
  , getEditContents
  , handleEditorEvent
  )
import Cursor (getCurrent, setCurrent)
import Data.Text (Text, unpack)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KEnter, KEsc))
import State.Changelist (Change(..), Operation(..), addChange)
import State.Types
  ( AppState(..)
  , EditorState(..)
  , FieldLifecycle(..)
  , ResourceName(..)
  )
import Trello.Types (TrelloEntry(editName))

handleEditorInput :: Event -> AppState -> EventM ResourceName (Next AppState)
handleEditorInput _ s@AppState { screen = [] } = continue s
handleEditorInput vtye s@AppState { screen = (screen:restScr)
                                  , screenEditor = editorStt@EditorState { editorField = editorField
                                                                         , fieldLifecycle = fieldLifecycle
                                                                         }
                                  , changes = changes
                                  } =
  case getCurrent screen of
    Just current ->
      let editedEntry =
            editName
              current
              (const . unpack . head . getEditContents $ editorField)
          nextState =
            continue $
            s
              { screenEditor =
                  editorStt
                    {showEditor = False, fieldLifecycle = UninitializedField}
              , screen = setCurrent editedEntry screen : restScr
              , changes =
                  case fieldLifecycle of
                    NewField -> Change Create editedEntry `addChange` changes
                    UpdatedField ->
                      Change Update editedEntry `addChange` changes
                    UninitializedField -> changes
              }
       in case vtye of
            EvKey KEnter [] -> nextState
            EvKey KEsc [] -> nextState
            _ -> do
              newEditor <- handleEditorEvent vtye editorField
              continue $ s {screenEditor = editorStt {editorField = newEditor}}
    Nothing -> continue s

simpleEditor :: Editor Text ResourceName
simpleEditor = editorText ItemEditor (Just 1) ""
