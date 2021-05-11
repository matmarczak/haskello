{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TrelloApi where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text as T
import Data.Text.Encoding
import Data.Text.IO as T
import Network.Wreq
import Parser

import System.Environment

boardsUrl :: String -> String -> String
boardsUrl key token =
  "https://api.trello.com/1/members/me/boards?fields=name&key=" ++
  key ++ "&token=" ++ token ++ "&filter=open&lists=open"

getBoards :: IO [Board]
getBoards = do
  key <- getEnv "TRELLO_KEY"
  token <- getEnv "TRELLO_TOKEN"
  r <- get $ boardsUrl key token
  let body = r ^. responseBody
      objs = eitherDecode body :: Either String [Board]
  case objs of
    Right bd -> return bd
    Left _ -> return []
