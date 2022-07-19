{-# LANGUAGE NamedFieldPuns #-}

module State.AppState where

import Control.Monad (join)
import Cursor
import Data.List (intercalate, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import State.Changelist (Change(..), Operation(..))
import State.Editor (simpleEditor)
import State.Types
import Trello.Api (getBoards)
import Trello.Types
  ( Board(..)
  , Card(cardId, cardIdList)
  , List(List, listId, listIdBoard)
  , TrelloEntry(..)
  , TrelloItem(..)
  )

addLists :: LocalTrello -> [List] -> LocalTrello
addLists t ls = t {lists = ls ++ lists t}

addCards :: LocalTrello -> [Card] -> LocalTrello
addCards t cs = t {listCards = cs ++ listCards t}

getChildren :: LocalTrello -> TrelloItem -> [TrelloItem]
getChildren trello parent =
  case parent of
    TBoard Board {boardId = boardId} ->
      map TList . filter ((boardId ==) . listIdBoard) $ lists trello
    TList List {listId = listId} ->
      map TCard . filter ((listId ==) . cardIdList) $ listCards trello
    _ -> []

getParents :: LocalTrello -> [TrelloItem] -> Maybe [TrelloItem]
getParents trello items =
  case head items of
    TBoard _ -> Nothing
    TList _ -> Just $ map TBoard $ boards trello
    TCard _ -> Just $ map TList $ lists trello

applyChange :: Change -> LocalTrello -> LocalTrello
applyChange change (LocalTrello boards lists cards) =
  let Change operation item = change
      updateFunc idGetter =
        case operation of
          Create -> (:)
          Update ->
            (\ch ->
               map
                 (\el ->
                    if idGetter el == idGetter ch
                      then ch
                      else el))
          Delete -> (\ch -> filter (\el -> idGetter el /= idGetter ch))
   in case item of
        TBoard b -> LocalTrello (updateFunc boardId b boards) lists cards
        TList l -> LocalTrello boards (updateFunc listId l lists) cards
        TCard c -> LocalTrello boards lists (updateFunc cardId c cards)

applyChanges :: [Change] -> LocalTrello -> LocalTrello
applyChanges changes trello = foldr applyChange trello changes

makeScreen :: [TrelloItem] -> Cursor TrelloItem
makeScreen = Cursor.fromList . sortOn pos

updateHeader :: AppState -> AppState
updateHeader s@AppState {screen = screens} =
  let context =
        case length screens of
          1 -> "Boards"
          2 -> "Lists"
          3 -> "Cards"
          _ -> ""
      getName x =
        (\name ->
           if length name > 30
             then (++ "...") $ take 30 name
             else name) .
        itemName <$>
        getCurrent x
      breadcrumbs =
        intercalate " / " $ catMaybes $ reverse $ tail $ map getName screens
   in s
        { header =
            (if breadcrumbs /= ""
               then breadcrumbs ++ " / "
               else "") ++
            context
        }

buildInitialState :: IO AppState
buildInitialState = do
  boards <- getBoards
  let editorState =
        EditorState
          { showEditor = False
          , editorField = simpleEditor
          , fieldLifecycle = UninitializedField
          }
  case boards of
    Left err ->
      return $
      AppState
        { serverData = LocalTrello [] [] []
        , screen = [makeScreen []]
        , screenEditor = editorState
        , changes = []
        , modal = Just err
        , header = "Boards"
        }
    Right fetchedBoards -> do
      let boardSorter = sortOn boardLastActivity
          lists = join $ map (fromMaybe [] . boardLists) fetchedBoards
          items = map TBoard . reverse . boardSorter $ fetchedBoards
      return $
        AppState
          { serverData = LocalTrello fetchedBoards lists []
          , screen = [makeScreen items]
          , screenEditor = editorState
          , changes = []
          , modal = Nothing
          , header = "Boards"
          }
