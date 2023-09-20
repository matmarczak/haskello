{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trello.Api where

import Control.Exception (catch)
import Control.Lens
import Data.Aeson
import Network.HTTP.Client
  ( HttpException(HttpExceptionRequest)
  , HttpExceptionContent(StatusCodeException)
  )

import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)

import Network.Wreq (Response, get, post, put, responseBody)
import Text.Printf (printf)
import Trello.Types (Board(..), Card(..), List(..), TrelloId(..))

import System.Environment

baseUrl :: String
baseUrl = "https://api.trello.com"

type PathParam = String

type UrlPath = String

data QueryParam =
  Param
    { keyParam :: String
    , valuesParam :: [String]
    }

data Endpoint =
  Endpoint
    { path :: UrlPath
    , pathParams :: [PathParam]
    , queryParams :: [QueryParam]
    }

paramClosed :: Bool -> QueryParam
paramClosed isClosed =
  Param
    "closed"
    [ if isClosed
        then "true"
        else "false"
    ]

membersBoardsEndpoint :: Endpoint
membersBoardsEndpoint =
  Endpoint
    { path = "/1/members/me/boards"
    , pathParams = []
    , queryParams =
        [ Param "fields" ["name", "closed", "dateLastActivity"]
        , Param "filter" ["open"]
        ]
    }

boardDetails :: String -> Endpoint
boardDetails boardId =
  Endpoint
    { path = "/1/board/%s"
    , pathParams = [boardId]
    , queryParams =
        [ Param "fields" ["id", "name", "closed", "idBoard", "dateLastActivity"]
        , Param "lists" ["open"]
        , Param "cards" ["open"]
        , Param "card_fields" ["id", "name", "idList", "pos", "closed"]
        ]
    }

listDetails :: String -> Endpoint
listDetails listId =
  Endpoint
    { path = "/1/lists/%s/cards"
    , pathParams = [listId]
    , queryParams =
        [ Param "fields" ["id", "name", "idList,", "closed", "pos"]
        , Param "lists" ["open"]
        , Param "closed" ["false"]
        ]
    }

renderUrl :: Endpoint -> String
renderUrl Endpoint { path = path
                   , queryParams = queryParams
                   , pathParams = pathParams
                   } =
  let renderedPath =
        case length pathParams of
          0 -> path
          1 -> printf path $ head pathParams
          _ -> "error"
      renderedQueryParams =
        foldl
          (\s p ->
             s ++
             (if null s
                then "?"
                else "&") ++
             keyParam p ++ "=" ++ intercalate "," (valuesParam p))
          ""
          queryParams
   in baseUrl ++ renderedPath ++ renderedQueryParams

renderWithCreds :: Endpoint -> IO String
renderWithCreds endpoint = do
  key <- getEnv "TRELLO_KEY"
  token <- getEnv "TRELLO_TOKEN"
  return . renderUrl $
    endpoint
      { queryParams =
          Param "token" [token] : Param "key" [key] : queryParams endpoint
      }

decodeEither :: FromJSON a => Response ByteString -> a -> IO a
decodeEither resp base =
  case eitherDecode $ resp ^. responseBody of
    Right payload -> return payload
    Left err -> do
      print err
      print resp
      return base

decodeEither2 :: FromJSON a => Response ByteString -> Either String a
decodeEither2 resp =
  case eitherDecode $ resp ^. responseBody of
    Right payload -> Right payload
    Left err -> Left err

getEither :: String -> IO (Either String (Response ByteString))
getEither url = do
  let handler (HttpExceptionRequest _ (StatusCodeException resp _)) =
        return $ Left $ "Url: " ++ url ++ "\n\nResponse:" ++ show resp
      handler err = return $ Left $ show err
  catch (Right <$> get url) handler

putEither :: String -> ByteString -> IO (Either String (Response ByteString))
putEither url body = do
  let handler (HttpExceptionRequest _ (StatusCodeException resp _)) =
        return $ Left $ "Url: " ++ url ++ "\n\nResponse:" ++ show resp
      handler err = return $ Left $ show err
  catch (Right <$> put url body) handler

postEither :: String -> ByteString -> IO (Either String (Response ByteString))
postEither url body = do
  let handler (HttpExceptionRequest _ (StatusCodeException resp _)) =
        return $ Left $ "Url: " ++ url ++ "\n\nResponse:" ++ show resp
      handler err = return $ Left $ show err
  catch (Right <$> post url body) handler

getItem :: FromJSON a => Endpoint -> IO (Either String a)
getItem endpoint = do
  urlWithCreds <- renderWithCreds endpoint
  resp <- getEither urlWithCreds
  return $ do
    v <- resp
    decodeEither2 v

putItem :: FromJSON a => Endpoint -> IO (Either String a)
putItem endpoint = do
  urlWithCreds <- renderWithCreds endpoint
  resp <- putEither urlWithCreds mempty
  return $ do
    v <- resp
    decodeEither2 v

postItem :: FromJSON a => Endpoint -> IO (Either String a)
postItem endpoint = do
  urlWithCreds <- renderWithCreds endpoint
  resp <- postEither urlWithCreds mempty
  return $ do
    v <- resp
    decodeEither2 v

getBoards :: IO (Either String [Board])
getBoards = getItem membersBoardsEndpoint

getBoard :: TrelloId -> IO (Either String Board)
getBoard (ServerId idBoard) = getItem $ boardDetails idBoard
getBoard (TemporaryId _) = return $ Left "Tried to GET board with temporary id"

getCards :: String -> IO (Either String [Card])
getCards idList = getItem $ listDetails idList

putCardUrl :: Card -> Either String Endpoint
putCardUrl card =
  case cardId card of
    ServerId cardServerId ->
      Right $
      Endpoint
        { path = "/1/cards/%s"
        , pathParams = [cardServerId]
        , queryParams =
            [ Param "name" [cardName card]
            , paramClosed $ cardClosed card
            , Param "pos" [show $ cardPos card]
            ]
        }
    TemporaryId _ -> Left "Tried to PUT card with temporary id"

putListUrl :: List -> Either String Endpoint
putListUrl list =
  case listId list of
    ServerId listServerId ->
      Right $
      Endpoint
        { path = "/1/lists/%s"
        , pathParams = [listServerId]
        , queryParams =
            [ Param "name" [listName list]
            , paramClosed $ listClosed list
            , Param "pos" [show . listPos $ list]
            ]
        }
    TemporaryId _ -> Left "Tried to PUT list with temporary id"

putBoardUrl :: Board -> Either String Endpoint
putBoardUrl board =
  case boardId board of
    ServerId boardServerId ->
      Right $
      Endpoint
        { path = "/1/board/%s"
        , pathParams = [boardServerId]
        , queryParams =
            [Param "name" [boardName board], paramClosed $ boardClosed board]
        }
    TemporaryId _ -> Left "Tried to PUT board with temporary id"

postBoardUrl :: Board -> Endpoint
postBoardUrl board =
  Endpoint
    { path = "/1/boards"
    , pathParams = []
    , queryParams =
        [Param "name" [boardName board], Param "defaultLists" ["false"]]
    }

postCardUrl :: Card -> Either String Endpoint
postCardUrl card =
  case cardIdList card of
    TemporaryId _ -> Left "Tried to POST card with parent temporary id"
    ServerId sid ->
      Right $
      Endpoint
        { path = "/1/cards"
        , pathParams = []
        , queryParams =
            [ Param "name" [cardName card]
            , Param "idList" [sid]
            , Param "pos" [show . cardPos $ card]
            ]
        }

postListUrl :: List -> Either String Endpoint
postListUrl list =
  case listIdBoard list of
    ServerId sid ->
      Right $
      Endpoint
        { path = "/1/lists"
        , pathParams = []
        , queryParams =
            [ Param "name" [listName list]
            , Param "idBoard" [sid]
            , Param "pos" [show . listPos $ list]
            ]
        }
    TemporaryId _ -> Left "Tried to POST list with parent temporary id"

updateCardChanges :: Card -> IO (Either String Card)
updateCardChanges card =
  case putCardUrl card of
    Left err -> return $ Left err
    Right endpoint -> putItem endpoint

updateListChanges :: List -> IO (Either String List)
updateListChanges list =
  case putListUrl list of
    Left err -> return $ Left err
    Right endpoint -> putItem endpoint

updateBoardChanges :: Board -> IO (Either String Board)
updateBoardChanges board =
  case putBoardUrl board of
    Left err -> return $ Left err
    Right endpoint -> putItem endpoint

postCard :: Card -> IO (Either String Card)
postCard card =
  case postCardUrl card of
    Left err -> return $ Left err
    Right endpoint -> postItem endpoint

postList :: List -> IO (Either String List)
postList list =
  case postListUrl list of
    Left err -> return $ Left err
    Right endpoint -> postItem endpoint

postBoard :: Board -> IO (Either String Board)
postBoard board = postItem (postBoardUrl board)

newtype State s a =
  State
    { runState :: s -> (a, s)
    }

instance Functor (State s) where
  fmap f (State g) =
    State $ \s ->
      let (val, newState) = g s
       in (f val, newState)

instance Applicative (State s) where
  pure val = State $ \s -> (val, s)
  (State f) <*> (State g) =
    State $ \s ->
      let (func, state) = f s
          (val, state2) = g state
       in (func val, state2)

instance Monad (State s) where
  (State a) >>= f =
    State $ \s ->
      let (val, newState) = a s
          (State g) = f val
       in g newState

put1 :: Int -> State [Int] Int
put1 val = State $ \s -> (val, val : s)

pop1 :: State [Int] Int
pop1 = State $ \(x:xs) -> (x, xs)
-- # how to pop everything from one list?
