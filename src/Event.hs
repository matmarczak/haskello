{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Event where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent(VtyEvent), EventM, Next)
import Brick.Widgets.Edit (applyEdit)
import Control.Monad.IO.Class (liftIO)
import Cursor
  ( Cursor
  , getCurrent
  , getNext
  , getPrev
  , insertNext
  , insertPrev
  , jumpDown
  , jumpUp
  , moveFirst
  , moveLast
  , moveNext
  , movePrev
  , relocateNext
  , relocatePrev
  , rmCurrent
  )
import qualified Data.List
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text.Zipper (textZipper)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty.Input.Events
import State.AppState
  ( addCards
  , addLists
  , applyChanges
  , buildInitialState
  , getChildren
  , makeScreen
  , updateHeader
  )
import State.Changelist
  ( Change(..)
  , Operation(..)
  , addChange
  , addChanges
  , updateChangelistChildren
  )
import State.Editor (handleEditorInput)
import State.Types
  ( AppState(..)
  , EditorState(showEditor)
  , EditorState(..)
  , FieldLifecycle(..)
  , ResourceName
  )
import Trello.Api
  ( getBoard
  , postBoard
  , postCard
  , postList
  , updateBoardChanges
  , updateCardChanges
  , updateListChanges
  )
import Trello.Types
  ( Board(..)
  , Card(..)
  , List(..)
  , TrelloEntry(..)
  , TrelloId(..)
  , TrelloItem(..)
  , setPos
  )

handleUIEventPostProcessed ::
     AppState
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next AppState)
handleUIEventPostProcessed = (fmap . fmap . fmap) updateHeader . handleUIEvent

handleUIEvent ::
     AppState
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next AppState)
handleUIEvent s@AppState {screen = []} _ =
  continue $ s {modal = Just "Unable to handle event with no screens"}
handleUIEvent s@AppState { screen = (screen:restScreens)
                         , changes = changes
                         , screenEditor = editorStt@EditorState { showEditor = showEditor
                                                                , editorField = editorField
                                                                }
                         , modal = modal
                         } e =
  case e of
    VtyEvent vtye ->
      case modal of
        Just _ ->
          let dismiss = continue $ s {modal = Nothing}
              EvKey k _ = vtye
           in case k of
                KEnter -> dismiss
                KEsc -> dismiss
                KChar 'q' -> dismiss
                _ -> continue s
        Nothing ->
          if showEditor
            then handleEditorInput vtye s
            else case vtye of
                   EvKey (KChar '?') [] ->
                     continue $ s {modal = Just $ renderShortcuts shortcuts}
                   EvKey (KChar 'q') [MCtrl] -> halt s
                   EvKey (KChar 'q') [] -> do
                     (shouldHalt, newState) <- liftIO $ saveStateToServer s
                     if shouldHalt
                       then do
                         halt s
                       else continue newState
                   EvKey (KChar 'l') [] -> handleOpenItem s
                   EvKey (KChar 'h') [] ->
                     continue $
                     case restScreens of
                       [] -> s
                       _ -> s {screen = restScreens}
                   EvKey (KChar 'k') [] ->
                     continue $
                     case movePrev screen of
                       Just prev -> s {screen = prev : restScreens}
                       Nothing -> s
                   EvKey (KChar 'K') [] ->
                     continue $
                     case relocatePrev screen of
                       Just relocated ->
                         s
                           { screen = relocated : restScreens
                           , changes =
                               getRelocatePrevChanges screen `addChanges`
                               changes
                           }
                       Nothing -> s
                   EvKey (KChar 'j') [] ->
                     continue $
                     case moveNext screen of
                       Just next -> s {screen = next : restScreens}
                       Nothing -> s
                   EvKey (KChar 'J') [] ->
                     continue $
                     case relocateNext screen of
                       Just relocated ->
                         s
                           { screen = relocated : restScreens
                           , changes =
                               getRelocateNextChanges screen `addChanges`
                               changes
                           }
                       Nothing -> s
                   EvKey (KChar 'i') [] ->
                     case getCurrent screen of
                       Nothing -> continue $ s {modal = Just "No item to edit"}
                       Just currentItem ->
                         let zipper =
                               const $
                               textZipper
                                 [fromString $ itemName currentItem]
                                 (Just 1)
                             newEditorText = applyEdit zipper editorField
                          in continue $
                             s
                               { screenEditor =
                                   editorStt
                                     { showEditor = True
                                     , editorField = newEditorText
                                     , fieldLifecycle =
                                         case itemId currentItem of
                                           TemporaryId _ -> NewField
                                           ServerId _ -> UpdatedField
                                     }
                               }
                   EvKey (KChar 'o') [] ->
                     insertItem insertNext getPosForInsertedBelow s
                   EvKey (KChar 'O') [] ->
                     insertItem insertPrev getPosForInsertedAbove s
                   EvKey (KChar 's') [] -> do
                     (_, newState) <- liftIO $ saveStateToServer s
                     continue newState
                   EvKey (KChar 'd') [] ->
                     case (getCurrent screen, rmCurrent screen) of
                       (Nothing, _) ->
                         continue $ s {modal = Just "No item to delete"}
                       (Just currentItem, Just newScreen) ->
                         continue $
                         s
                           { screen = newScreen : restScreens
                           , changes =
                               Change Delete (close currentItem) `addChange`
                               changes
                           }
                       _ -> continue $ s {modal = Just "Unable to delete item"}
                   EvKey (KChar 'g') [] ->
                     continue $ s {screen = moveFirst screen : restScreens}
                   EvKey (KChar 'G') [] ->
                     continue $ s {screen = moveLast screen : restScreens}
                   EvKey (KChar 'd') [MCtrl] ->
                     continue $
                     s
                       { screen =
                           (case jumpDown 10 screen of
                              Just newScr -> newScr
                              Nothing -> moveLast screen) :
                           restScreens
                       }
                   EvKey (KChar 'u') [MCtrl] ->
                     continue $
                     s
                       { screen =
                           (case jumpUp 10 screen of
                              Just newScr -> newScr
                              Nothing -> moveFirst screen) :
                           restScreens
                       }
                   EvKey (KChar 'b') [] -> do
                     liftIO $ print changes
                     continue s
                   _ -> continue s
    _ -> continue s

saveChange :: Change -> IO (Either String TrelloItem)
saveChange change =
  let Change op item = change
   in case op of
        Create ->
          case item of
            TBoard b -> fmap TBoard <$> postBoard b
            TCard c -> fmap TCard <$> postCard c
            TList l -> fmap TList <$> postList l
        _ ->
          case item of
            TBoard b -> fmap TBoard <$> updateBoardChanges b
            TList l -> fmap TList <$> updateListChanges l
            TCard c -> fmap TCard <$> updateCardChanges c

getPosForInsertedBelow :: Cursor TrelloItem -> Float
getPosForInsertedBelow c =
  case (getCurrent c, getNext c) of
    (Just currentItem, Just nextItem) -> (pos currentItem + pos nextItem) / 2
    (Just currentItem, Nothing) -> pos currentItem * 2
    (Nothing, _) -> 0

getPosForInsertedAbove :: Cursor TrelloItem -> Float
getPosForInsertedAbove c =
  case (getCurrent c, getPrev c) of
    (Just currentItem, Just prevItem) -> (pos currentItem + pos prevItem) / 2
    (Just currentItem, Nothing) -> pos currentItem / 2
    (Nothing, _) -> error "Should not be called without current item"

insertItem ::
     (TrelloItem -> Cursor TrelloItem -> Cursor TrelloItem)
  -> (Cursor TrelloItem -> Float)
  -> AppState
  -> EventM ResourceName (Next AppState)
insertItem _ _ s@AppState {screen = []} =
  continue $ s {modal = Just "Unable to insert with no screen"}
insertItem inserter posGetter s@AppState { screen = screens@(screen:restScreens)
                                         , screenEditor = EditorState {editorField = editorField}
                                         } =
  let itemType =
        case length screens of
          0 -> "board"
          1 -> "board"
          2 -> "list"
          3 -> "card"
          _ -> "error"
      newPos = posGetter screen
      zipper = const $ textZipper [""] (Just 1)
      newEditorText = applyEdit zipper editorField
      newState trelloItem =
        s
          { screenEditor =
              EditorState
                { showEditor = True
                , editorField = newEditorText
                , fieldLifecycle = NewField
                }
          , screen = inserter trelloItem screen : restScreens
          }
   in do tsStr <- show <$> liftIO getPOSIXTime
         case (itemType :: String) of
           "board" ->
             continue $
             newState
               (TBoard $
                Board
                  { boardId = TemporaryId tsStr
                  , boardName = ""
                  , boardCards = Nothing
                  , boardLists = Nothing
                  , boardClosed = False
                  , boardLastActivity = Just ""
                  })
           "list" ->
             case getCurrent $ head restScreens of
               Nothing ->
                 continue $ s {modal = Just "Unable to detect parent id"}
               Just parentBoard ->
                 continue $
                 newState
                   (TList $
                    List
                      { listCards = Nothing
                      , listClosed = False
                      , listId = TemporaryId tsStr
                      , listIdBoard = itemId parentBoard
                      , listName = ""
                      , listPos = newPos
                      })
           "card" ->
             case getCurrent $ head restScreens of
               Nothing ->
                 continue $ s {modal = Just "Unable to detect parent id"}
               Just currentItem ->
                 continue $
                 newState
                   (TCard $
                    Card
                      { cardClosed = False
                      , cardId = TemporaryId tsStr
                      , cardIdList = itemId currentItem
                      , cardName = ""
                      , cardPos = newPos
                      })
           "error" -> continue $ s {modal = Just "Invalid state of screens"}
           _ -> continue s

getRelocateNextChanges :: Cursor TrelloItem -> [Change]
getRelocateNextChanges c =
  case (getCurrent c, getNext c) of
    (Nothing, _) -> []
    (_, Nothing) -> []
    (Just currentItem, Just nextItem) ->
      [ Change Update $ setPos currentItem $ pos nextItem
      , Change Update $ setPos nextItem $ pos currentItem
      ]

getRelocatePrevChanges :: Cursor TrelloItem -> [Change]
getRelocatePrevChanges c =
  case (getCurrent c, getPrev c) of
    (Nothing, _) -> []
    (_, Nothing) -> []
    (Just currentItem, Just prevItem) ->
      [ Change Update $ setPos currentItem $ pos prevItem
      , Change Update $ setPos prevItem $ pos currentItem
      ]

handleOpenItem :: AppState -> EventM ResourceName (Next AppState)
handleOpenItem s@AppState {screen = []} =
  continue $ s {modal = Just "Unable to open with no screen"}
handleOpenItem s@AppState { screen = screens@(screen:_)
                          , changes = changes
                          , serverData = serverData
                          } =
  case getCurrent screen of
    Nothing -> continue $ s {modal = Just "No item to access"}
    Just current ->
      let children = getChildren (applyChanges changes serverData) current
       in case current of
            (TBoard b) ->
              case boardId b of
                TemporaryId _ ->
                  continue $
                  let newScreen =
                        makeScreen $
                        getChildren (applyChanges changes serverData) current
                   in s {screen = newScreen : screens}
                ServerId _ ->
                  case children of
                    [] -> do
                      response <- liftIO $ getBoard $ boardId b
                      case response of
                        Left err -> continue s {modal = Just err}
                        Right fetchedBoard ->
                          let dataWithLists =
                                addLists serverData $
                                fromMaybe [] $ boardLists fetchedBoard
                              dataWithCards =
                                addCards
                                  dataWithLists
                                  (fromMaybe [] $ boardCards fetchedBoard)
                              newScreen =
                                makeScreen $
                                getChildren
                                  (applyChanges changes dataWithCards)
                                  current
                           in continue $
                              s
                                { serverData = dataWithCards
                                , screen = newScreen : screens
                                }
                    lists -> continue $ s {screen = makeScreen lists : screens}
            TList _ -> continue $ s {screen = makeScreen children : screens}
            TCard _ -> continue s {modal = Just "No item to open"}

saveStateToServer :: AppState -> IO (Bool, AppState)
saveStateToServer state@AppState {changes = changes} =
  let orderedChanges =
        Data.List.sortOn
          ((\(Change _ titem) ->
              case titem of
                TBoard _ -> 1
                TList _ -> 2
                TCard _ -> 3) :: Change -> Integer)
          changes
   in case orderedChanges of
        [] -> return (True, state)
        _ -> do
          result <- liftIO $ rollSaveChanges (Right ([], orderedChanges))
          case result of
            Left (err, leftChanges) -> do
              newState <- liftIO buildInitialState
              return (False, newState {changes = leftChanges, modal = Just err})
            Right (_, []) -> do
              newState <- liftIO buildInitialState
              return
                ( True
                , newState
                    {changes = [], modal = Just "Changes saved successfully!"})
            Right (_, _) ->
              error
                "Any unsaved changes should result in Left. Got Right instead!"

type SyncedChanges = Either (String, [Change]) ([TrelloItem], [Change])

rollSaveChanges :: SyncedChanges -> IO SyncedChanges
rollSaveChanges s@(Left _) = return s
rollSaveChanges s@(Right (_, [])) = return s
rollSaveChanges (Right (items, fstChange:rest)) = do
  synced <- saveChange fstChange
  case synced of
    Left err -> return $ Left (err, fstChange : rest)
    Right res ->
      let (Change _ oldItem) = fstChange
       in rollSaveChanges $
          Right (res : items, updateChangelistChildren oldItem res rest)

renderShortcuts :: [(String, String)] -> String
renderShortcuts shortcutsList =
  intercalate "\n" $ fmap (\(k, v) -> k ++ " - " ++ v) shortcutsList

shortcuts :: [(String, String)]
shortcuts =
  [ ("j", "move cursor down")
  , ("k", "move cursor up")
  , ("h", "exit item")
  , ("l", "enter item")
  , ("c-u", "jump up")
  , ("c-d", "jump down")
  , ("g", "jump to start")
  , ("G", "jump to end")
  , ("K", "reorder item up")
  , ("J", "reorder item down")
  , ("d", "delete item changes")
  , ("i", "edit item")
  , ("o", "add item below")
  , ("O", "add item above")
  , ("s", "save changes")
  , ("q", "save and quit")
  , ("c-q", "force quit (no save)")
  ]
