{-# LANGUAGE NamedFieldPuns #-}

module Cursor
  ( Cursor
  , fromList
  , jump
  , jumpDown
  , jumpUp
  , movePrev
  , moveNext
  , hasPrev
  , hasNext
  , insertPrev
  , insertNext
  , getCurrent
  , relocatePrev
  , relocateNext
  , getPrev
  , setCurrent
  , getNext
  , rmCurrent
  , allItems
  , allByOrder
  , moveFirst
  , moveLast
  ) where

import Data.List (unfoldr)

data Cursor a
  = Empty
  | Cursor
      { prev :: [a]
      , curr :: a
      , next :: [a]
      }
  deriving (Show, Eq)

data Direction
  = Up
  | Down
  deriving (Show)

_fromList :: [a] -> Cursor a
_fromList [] = Empty
_fromList (x:xs) = Cursor {prev = [], curr = x, next = xs}

hasNext :: Cursor a -> Bool
hasNext Cursor {next} = not $ null next
hasNext Empty = False

hasPrev :: Cursor a -> Bool
hasPrev Cursor {prev} = not $ null prev
hasPrev Empty = False

isEmpty :: Cursor a -> Bool
isEmpty Empty = True
isEmpty _ = False

moveNext :: Cursor a -> Maybe (Cursor a)
moveNext c
  | isEmpty c = Nothing
  | hasNext c =
    let (newCurr:newNext) = next c
     in Just $ Cursor {prev = curr c : prev c, curr = newCurr, next = newNext}
  | otherwise = Nothing

movePrev :: Cursor a -> Maybe (Cursor a)
movePrev c
  | isEmpty c = Nothing
  | hasPrev c =
    let (newCurr:newPrev) = prev c
     in Just $ Cursor {prev = newPrev, curr = newCurr, next = curr c : next c}
  | otherwise = Nothing

removeNext :: Cursor a -> Maybe (Cursor a)
removeNext c
  | isEmpty c = Nothing
  | hasNext c = Just $ c {next = (tail . next) c}
  | otherwise = Nothing

removePrev :: Cursor a -> Maybe (Cursor a)
removePrev c
  | isEmpty c = Nothing
  | hasPrev c = Just $ c {prev = (tail . prev) c}
  | otherwise = Nothing

insertNext :: a -> Cursor a -> Cursor a
insertNext newCurr c
  | isEmpty c = _fromList [newCurr]
  | otherwise = c {prev = curr c : prev c, curr = newCurr}

insertPrev :: a -> Cursor a -> Cursor a
insertPrev newCurr c
  | isEmpty c = _fromList [newCurr]
  | otherwise = c {curr = newCurr, next = curr c : next c}

setCurrent :: a -> Cursor a -> Cursor a
setCurrent item c
  | isEmpty c = _fromList [item]
  | otherwise = c {curr = item}

getCurrent :: Cursor a -> Maybe a
getCurrent c
  | isEmpty c = Nothing
  | otherwise = Just $ curr c

moveLast :: Cursor a -> Cursor a
moveLast c = maybe c moveLast (moveNext c)

moveFirst :: Cursor a -> Cursor a
moveFirst c = maybe c moveFirst (movePrev c)

allByOrder :: Cursor a -> ([a], [a], [a])
allByOrder c =
  let makeGen mover getter c' = ((,) $ head $ getter c') <$> mover c'
      prevs = unfoldr (makeGen movePrev prev) c
      current = maybe [] (: []) (getCurrent c)
      nexts = unfoldr (makeGen moveNext next) c
   in (reverse prevs, current, nexts)

allItems :: Cursor a -> [a]
allItems c =
  let (prevs, current, nexts) = allByOrder c
   in prevs ++ current ++ nexts

rmCurrent :: Cursor a -> Maybe (Cursor a)
rmCurrent c
  | isEmpty c = Nothing
  | otherwise =
    case (movePrev c, moveNext c) of
      (Nothing, Just nextCursor) -> removePrev nextCursor
      (Just prevCursor, Nothing) -> removeNext prevCursor
      (Just _, Just nextCursor) -> removePrev nextCursor
      (Nothing, Nothing) -> Just Empty

getNext :: Cursor a -> Maybe a
getNext c = moveNext c >>= getCurrent

getPrev :: Cursor a -> Maybe a
getPrev c = movePrev c >>= getCurrent

relocateNext :: Cursor a -> Maybe (Cursor a)
relocateNext c = do
  item <- getNext c
  withoutNext <- removeNext c
  let swappedCursor = insertPrev item withoutNext
  moveNext swappedCursor

relocatePrev :: Cursor a -> Maybe (Cursor a)
relocatePrev c = do
  item <- getPrev c
  withoutPrev <- removePrev c
  let swappedCursor = insertNext item withoutPrev
  movePrev swappedCursor

jump :: (Cursor a -> Maybe (Cursor a)) -> Int -> Cursor a -> Maybe (Cursor a)
jump step n c = do
  newC <- step c
  if n > 1
    then jump step (n - 1) newC
    else return newC

jumpDown :: Int -> Cursor a -> Maybe (Cursor a)
jumpDown = jump moveNext

jumpUp :: Int -> Cursor a -> Maybe (Cursor a)
jumpUp = jump movePrev

fromList :: [a] -> Cursor a
fromList = _fromList
