{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text as T
import Data.Word
import GHC.Generics

data Card =
  Card
    { cardName :: String
    , cardId :: String
    , cardDesc :: String
    , cardIdList :: String
    }
  deriving (Show, Generic)

data CardList =
  CardList
    { cardListName :: String
    , cardListId :: String
    , cardListBoardId :: String
    }
  deriving (Show, Generic)

data Board =
  Board
    { boardName :: String
    , boardId :: String
    , boardLists :: Maybe [CardList]
    , boardCards :: Maybe [Card]
    }
  deriving (Show, Generic)

instance FromJSON Card where
  parseJSON =
    withObject "Card" $ \v ->
      Card <$> v .: "name" <*> v .: "id" <*> v .: "desc" <*> v .: "idList"

instance FromJSON CardList where
  parseJSON =
    withObject "CardList" $ \v ->
      CardList <$> v .: "name" <*> v .: "id" <*> v .: "idBoard"

instance FromJSON Board where
  parseJSON =
    withObject "Board" $ \v ->
      Board <$> v .: "name" <*> v .: "id" <*> v .:? "lists" <*> v .:? "cards"
