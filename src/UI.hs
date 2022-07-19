{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI where

import Brick.Types (ViewportType(Vertical), Widget)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Core
  ( hBox
  , str
  , strWrap
  , strWrapWith
  , txt
  , vBox
  , vLimit
  , viewport
  , visible
  , withAttr
  )
import Brick.Widgets.Dialog (dialog, renderDialog)
import Brick.Widgets.Edit (Editor, renderEditor)
import Cursor (Cursor, allByOrder)
import Data.Text (Text)
import State.Types
  ( AppState(..)
  , EditorState(..)
  , ResourceName(..)
  , editorField
  )
import Text.Wrap (WrapSettings(..), defaultWrapSettings)
import Trello.Types (TrelloItem, itemName)

makeViewport :: String -> Widget ResourceName -> Widget ResourceName
makeViewport label =
  Border.borderWithLabel (str label) . viewport MainViewport Vertical

drawScreen :: AppState -> [Widget ResourceName]
drawScreen AppState {screen = []} =
  [makeViewport "" $ str "Unable to get screen"]
drawScreen AppState { screen = screens@(screen:_)
                    , screenEditor = screenEditor
                    , modal = modal
                    , header = header
                    } =
  (case modal of
     Just contents -> [drawDialog $ contents ++ "\n\nEnter - dismiss\n? - help"]
     Nothing -> []) ++
  [makeViewport header . vBox $ drawItems screen screenEditor]

drawDialog :: String -> Widget n
drawDialog =
  vLimit 90 .
  renderDialog (dialog (Just "Hey!") Nothing 100) .
  strWrapWith (defaultWrapSettings {breakLongWords = True})

withActive :: Widget a -> Widget a
withActive = withAttr "active"

drawRow :: Char -> Widget ResourceName -> Widget ResourceName
drawRow prefixChar content = hBox [str (prefixChar : " "), content]

drawItems :: Cursor TrelloItem -> EditorState -> [Widget ResourceName]
drawItems cursor editor =
  let mapItems = map $ drawRow '*' . strWrap . itemName
      active = visible . withActive . drawRow '>' . strWrap . itemName
      currItemModifier =
        if showEditor editor
          then const . drawEditor $ editorField editor
          else active
      (prevs, current, nexts) = allByOrder cursor
   in mapItems prevs ++ map currItemModifier current ++ mapItems nexts

drawEditor :: (Ord n, Show n) => Editor Text n -> Widget n
drawEditor txtEditor =
  hBox $ map withActive [txt "> ", renderEditor (vBox . map txt) True txtEditor]
