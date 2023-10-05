module State.Types where

import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor)
import Cursor (Cursor)
import Data.Text (Text)
import State.Changelist (Change)
import Trello.Types (Board, Card, List, TrelloItem)

data ResourceName
  = MainViewport
  | ItemEditor
  deriving (Eq, Ord, Show)

data FieldLifecycle
  = UninitializedField
  | NewField
  | UpdatedField
  deriving (Show)

data EditorState =
  EditorState
    { showEditor :: Bool
    , editorField :: Editor Text ResourceName
    , fieldLifecycle :: FieldLifecycle
    }
  deriving (Show)

data AppState =
  AppState
    { serverData :: LocalTrello
    , screen :: [Cursor TrelloItem]
    , screenEditor :: EditorState
    , changes :: [Change]
    , lockedChanges :: [Change]
    , modal :: Maybe String
    , header :: String
    , bchan :: BChan HaskelloEvent
    }
  -- deriving (Show)

data LocalTrello =
  LocalTrello
    { boards :: [Board]
    , lists :: [List]
    , listCards :: [Card]
    }
  deriving (Show)

type SyncedChanges
   = Either (String, [(Change, TrelloItem)]) ([(Change, TrelloItem)], [Change])

data HaskelloEvent
  = AsyncHttpSaveToServer
  | AsyncHttpSaveResult SyncedChanges
