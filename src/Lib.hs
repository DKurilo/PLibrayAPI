{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Lib
    ( startApp
    , app
    , initConnectionPool
    ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types                 (Parser)
import           Data.ByteString                  (ByteString)
import           Data.Char                        (isDigit)
import           Data.Pool
import           Data.Text                        (Text, pack, unpack)
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Database.PostgreSQL.Simple       (Connection, Only (..), close,
                                                   connectPostgreSQL, fromOnly,
                                                   query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant
import           System.Environment               (lookupEnv)

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

type PutAccepted = Verb 'PUT 202

data ISBN = WrongISBN | ISBN10 String | ISBN13 String deriving (Eq)

isbnFromString :: String -> ISBN
isbnFromString cs | l == 10 && checkSum10ISBN cs' = ISBN10 cs
                  | l == 13 && checkSum13ISBN cs' = ISBN13 cs
                  | otherwise = WrongISBN
    where cs' = filter isDigit cs
          l = length cs'

checkSum10ISBN :: String -> Bool
checkSum10ISBN = (==0) . (`mod` 11) . foldr (\(i, c) s -> (11 - i) * (read [c] :: Int) + s) 0 . zip [1..10] . filter isDigit

checkSum13ISBN :: String -> Bool
checkSum13ISBN = (==0) . (`mod` 10) . foldr (\(i, c) s -> i * (read [c] :: Int) + s) 0 . zip oneThree . filter isDigit
    where oneThree = join $ repeat [1,3]

isbnToString :: ISBN -> String
isbnToString (ISBN13 cs) = cs
isbnToString (ISBN10 cs) = cs

instance Show ISBN where
    show = isbnToString

instance FromJSON ISBN where
  parseJSON v =
    isbnFromString . unpack <$> (parseJSON v :: Parser Text)

instance ToJSON ISBN where
  toJSON (ISBN13 cs) = toJSON . pack $ cs
  toJSON (ISBN10 cs) = toJSON . pack $ cs
  toJSON WrongISBN   = toJSON ("" :: Text)

newtype PostBook = PostBook ISBN

$(deriveJSON defaultOptions ''PostBook)

data Book = Book { id   :: Int
                 , isbn :: ISBN
                 }

$(deriveJSON defaultOptions ''Book)

data OverdueBook = OverdueBook { obid   :: Int
                               , obisbn :: ISBN
                               , obdays :: Int
                               , obuser :: Int
                               }

instance ToJSON OverdueBook where
    toJSON ob = object [ "id" .= (show . obid) ob
                       , "isbn" .= (show . obisbn) ob
                       , "daysOverdue" .= (show . obdays) ob
                       , "user" .= (show . obuser) ob
                       ]

type BookId = Int

type API = "api" :> "v1" :> "health" :> Get '[JSON] String
      :<|> "api" :> "v1" :> "librarian" :> "books" :> ReqBody '[JSON] PostBook :> PostCreated '[JSON] Book
      :<|> "api" :> "v1" :> "librarian" :> "books" :> ReqBody '[JSON] PostBook :> DeleteAccepted '[JSON] BookId
      :<|> "api" :> "v1" :> "librarian" :> "books" :> "overdue" :> Get '[JSON] [OverdueBook]

startApp :: IO ()
startApp = do
    envConnStr <- lookupEnv "POSTGRES_CONN"
    let connStr = case envConnStr of
                      Just cs -> (encodeUtf8 . pack) cs
                      _       -> ""
    pool <- initConnectionPool connStr
    runTLS tlsOpts warpOpts (app pool)
    where tlsOpts = tlsSettings "cert.pem" "key.pem"
          warpOpts = setPort 8080 defaultSettings

app :: Pool Connection -> Application
app pool = serve api (server pool)

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = health
          :<|> createBook conns
          :<|> deleteBook conns
          :<|> overdueBooks conns

createBook :: Pool Connection -> PostBook -> Handler Book
createBook conns (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                INSERT INTO book (isbn)
                VALUES (?)
                RETURNING book_id
                |] (Only . isbnToString $ postISBN)
            case rows of
                []           -> throwError db500Err
                Only uid : _ -> return $ Book uid postISBN
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db500Err = err500 { errBody = "Failed to create book" }

deleteBook :: Pool Connection -> PostBook -> Handler Int
deleteBook conns (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                DELETE FROM book
                WHERE book_id = (SELECT book_id FROM book WHERE ISBN = ? AND book_id NOT IN (SELECT book_id FROM account_book) LIMIT 1)
                RETURNING book_id
                |] (Only . isbnToString $ postISBN)
            case rows of
                []           -> throwError db404Err
                Only uid : _ -> return uid
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db404Err = err500 { errBody = "Can't delete book" }

overdueBooks :: Pool Connection -> Handler [OverdueBook]
overdueBooks conns = do
    rows <- liftIO $ withResource conns $ \conn -> query_ conn [sql|
                SELECT book.book_id,
                       book.ISBN,
                       EXTRACT(epoch from date_trunc('day', now()) - date_trunc('day', account_book.end_date)) / 3600 / 24,
                       account_book.user_id
                FROM  account_book
                INNER JOIN book
                ON account_book.book_id = book.book_id
                WHERE date_trunc('day', now()) > date_trunc('day', account_book.end_date)
                |]
    return $ map (\(bid, bookISBN, days, uid) -> OverdueBook bid (isbnFromString bookISBN) days uid) rows

health :: Handler String
health = liftIO $ show . (round . (* 1000)) <$> getPOSIXTime
