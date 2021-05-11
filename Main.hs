module Main where

import Data.Bits (xor)
import Foreign.C.Types
import Parser
import TrelloApi
import UI.NCurses

main :: IO ()
main = do
  boards <- getBoards
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    highlighted <- newColorID ColorBlack ColorYellow 1
    w <- defaultWindow
    let items = first {isActive = True} : rest
          where
            items' =
              let getItem x = flip Item False (boardName x) (boardId x)
               in map getItem boards
            first:rest = items'
    let (itemsUpdate, newItems) = updateItems items Keep highlighted
    updateWindow w $ itemsUpdate
    render
    waitFor w newItems

data Item =
  Item
    { getText :: String
    , isActive :: Bool
    , itemId :: String
    }
  deriving (Show)

data Direction
  = Up -- move cursor Up
  | Down -- move cursor Down
  | Keep -- keep cursor in place
  deriving (Show)

moveActive :: Direction -> [Item] -> [Item]
moveActive Up (x:y:xs) =
  if isActive y
    then x {isActive = True} : y {isActive = False} : xs
    else x : (moveActive Up $ y : xs)
moveActive Down (x:y:xs) =
  if isActive x
    then x {isActive = False} : y {isActive = True} : xs
    else x : (moveActive Down $ y : xs)
moveActive _ items = items

waitFor :: Window -> [Item] -> Curses ()
waitFor w items = loop items
  where
    loop items = do
      highlighted <- newColorID ColorBlack ColorYellow 1
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop items
        Just ev' ->
          let renderInWin = rerenderItems w highlighted
           in case ev' of
                EventCharacter 'q' -> return ()
                EventCharacter 'Q' -> return ()
                EventCharacter '\ESC' -> return ()
                EventSpecialKey KeyUpArrow -> do
                  newItems <- renderInWin Up items
                  loop newItems
                EventSpecialKey KeyDownArrow -> do
                  newItems <- renderInWin Down items
                  loop newItems
                EventCharacter '\n'
                 -> do
                  return ()
                _ -> loop items

rerenderItems :: Window -> ColorID -> Direction -> [Item] -> Curses [Item]
rerenderItems w highlighted direction items = do
  let (itemsUpdate, newItems) = updateItems items direction highlighted
  updateWindow w $ do
    moveCursor 0 3
    itemsUpdate
  render
  return newItems

updateItems :: [Item] -> Direction -> ColorID -> (Update (), [Item])
updateItems items direction highlighted =
  let update = do
        moveCursor 0 3
        updateList newList
   in (update, newList)
  where
    newList :: [Item]
    newList =
      case direction of
        Up -> moveActive Up items
        Down -> moveActive Down items
        Keep -> items
    updateList :: [Item] -> Update ()
    updateList items = sequence_ $ map (\(item) -> singleUpdate item) items
    singleUpdate :: Item -> Update ()
    singleUpdate item = do
      (row, col) <- cursorPosition
      moveCursor (row + 1) 3
      let drawer =
            if isActive item
              then drawColored highlighted
              else drawString
      drawer $ getText item

drawColored :: ColorID -> String -> Update ()
drawColored color itemText = do
  setColor color
  drawString itemText
  setColor defaultColorID
