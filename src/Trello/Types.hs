{-# LANGUAGE OverloadedStrings #-}

module Trello.Types where

import Data.Aeson

data TrelloId
  = TemporaryId String
  | ServerId String
  deriving (Show, Eq)

data Card =
  Card
    { cardName :: String
    , cardId :: TrelloId
    , cardIdList :: TrelloId
    , cardClosed :: Bool
    , cardPos :: Float
    }
  deriving (Show, Eq)

instance FromJSON Card where
  parseJSON =
    withObject "Card" $ \v ->
      Card <$> v .: "name" <*> (ServerId <$> v .: "id") <*>
      (ServerId <$> v .: "idList") <*>
      v .: "closed" <*>
      v .: "pos"

data List =
  List
    { listName :: String
    , listId :: TrelloId
    , listIdBoard :: TrelloId
    , listCards :: Maybe [Card]
    , listClosed :: Bool
    , listPos :: Float
    }
  deriving (Show, Eq)

instance FromJSON List where
  parseJSON =
    withObject "List" $ \v ->
      List <$> v .: "name" <*> (ServerId <$> v .: "id") <*>
      (ServerId <$> v .: "idBoard") <*>
      v .:? "listCards" <*>
      v .: "closed" <*>
      v .: "pos"

data Board =
  Board
    { boardName :: String
    , boardId :: TrelloId
    , boardCards :: Maybe [Card]
    , boardLists :: Maybe [List]
    , boardClosed :: Bool
    , boardLastActivity :: Maybe String
    }
  deriving (Show, Eq)

instance FromJSON Board where
  parseJSON =
    withObject "Board" $ \v ->
      Board <$> v .: "name" <*> (ServerId <$> v .: "id") <*> v .:? "cards" <*>
      v .:? "lists" <*>
      v .: "closed" <*>
      v .:? "dateLastActivity"

class TrelloEntry a where
  itemName :: a -> String
  itemId :: a -> TrelloId
  itemClosed :: a -> Bool
  editName :: a -> (String -> String) -> a
  eqId :: a -> a -> Bool
  eqId a1 a2 = itemId a1 == itemId a2
  dataChanged :: a -> a -> Bool
  dataChanged a1 a2 =
    itemName a1 /= itemName a2 || itemClosed a1 /= itemClosed a2
  close :: a -> a
  pos :: a -> Float

data TrelloItem
  = TCard Card
  | TList List
  | TBoard Board
  deriving (Show, Eq)

setPos :: TrelloItem -> Float -> TrelloItem
setPos item newPos =
  case item of
    TList l -> TList $ l {listPos = newPos}
    TCard c -> TCard $ c {cardPos = newPos}
    _ -> item

instance TrelloEntry TrelloItem where
  itemName (TCard c) = cardName c
  itemName (TList l) = listName l
  itemName (TBoard b) = boardName b
  itemId (TCard c) = cardId c
  itemId (TList l) = listId l
  itemId (TBoard b) = boardId b
  editName (TCard c) modifier = TCard $ c {cardName = modifier $ cardName c}
  editName (TList l) modifier = TList $ l {listName = modifier $ listName l}
  editName (TBoard b) modifier = TBoard $ b {boardName = modifier $ boardName b}
  close (TCard c) = TCard $ c {cardClosed = True}
  close (TList l) = TList $ l {listClosed = True}
  close (TBoard b) = TBoard $ b {boardClosed = True}
  itemClosed (TCard c) = cardClosed c
  itemClosed (TList l) = listClosed l
  itemClosed (TBoard b) = boardClosed b
  pos (TCard c) = cardPos c
  pos (TList l) = listPos l
  pos (TBoard _) = 0
