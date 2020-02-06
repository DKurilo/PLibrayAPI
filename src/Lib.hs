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
    , getConnectionString
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
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           System.Environment               (lookupEnv)
import           Types

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

startApp :: IO ()
startApp = do
    connStr <- getConnectionString
    pool <- initConnectionPool connStr
    runTLS tlsOpts warpOpts (app pool)
    where tlsOpts = tlsSettings "cert.pem" "key.pem"
          warpOpts = setPort 8080 defaultSettings

getConnectionString :: IO DBConnectionString
getConnectionString = do
    envConnStr <- lookupEnv "POSTGRES_CONN"
    return $ case envConnStr of
                 Just cs -> (encodeUtf8 . pack) cs
                 _       -> ""

app :: Pool Connection -> Application
app pool = serveWithContext api (genAuthServerContext pool) (server pool)

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = health
          :<|> createBook conns
          :<|> deleteBook conns
          :<|> overdueBooks conns
          :<|> checkedOutBooks conns
          :<|> checkOutBook conns
          :<|> returnBook conns

createBook :: Pool Connection -> AuthLibrarian -> PostBook -> Handler Book
createBook conns _ (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                INSERT INTO book (isbn)
                VALUES (?)
                RETURNING book_id
                |] (Only . isbnToString $ postISBN)
            case rows of
                []           -> throwError db500Err
                Only bid : _ -> return $ Book bid postISBN
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db500Err = err500 { errBody = "Failed to create book" }

deleteBook :: Pool Connection -> AuthLibrarian -> PostBook -> Handler Int
deleteBook conns _ (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                DELETE FROM book
                WHERE book_id = (SELECT book_id FROM book WHERE ISBN = ? AND book_id NOT IN (SELECT book_id FROM account_book) LIMIT 1)
                RETURNING book_id
                |] (Only . isbnToString $ postISBN)
            case rows of
                []           -> throwError db404Err
                Only bid : _ -> return bid
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db404Err = err404 { errBody = "Can't delete book" }

overdueBooks :: Pool Connection -> AuthLibrarian -> Handler [OverdueBook]
overdueBooks conns _ = do
    rows <- liftIO $ withResource conns $ \conn -> query_ conn [sql|
                SELECT book.book_id,
                       book.ISBN,
                       floor(EXTRACT(epoch from date_trunc('day', now()) - date_trunc('day', account_book.end_date)) / 3600 / 24)::integer,
                       account_book.user_id
                FROM  account_book
                INNER JOIN book
                ON account_book.book_id = book.book_id
                WHERE date_trunc('day', now()) > date_trunc('day', account_book.end_date)
                |]
    return $ map (\(bid, bookISBN, days, uid) -> OverdueBook bid (isbnFromString bookISBN) days uid) rows

checkedOutBooks :: Pool Connection -> AuthUser -> Handler [CheckedOutBook]
checkedOutBooks conns au = do
    rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT book.book_id,
                       book.ISBN,
                       account_book.check_date,
                       account_book.end_date,
                       floor(EXTRACT(epoch from date_trunc('day', account_book.end_date) - date_trunc('day', now())) / 3600 / 24)::integer
                FROM  account_book
                INNER JOIN book
                ON account_book.book_id = book.book_id
                WHERE account_book.user_id = ?
                |] ((Only . unAuthUser) au)
    return $ map (\(bid, bookISBN, coDate, eDate, days) -> CheckedOutBook bid (isbnFromString bookISBN) coDate eDate days) rows

checkOutBook :: Pool Connection -> AuthUser -> PostBook -> Handler CheckedOutBook
checkOutBook conns au (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            let csISBN = isbnToString postISBN
                uid = unAuthUser au
            checkUsersOverdued conns uid
            checkCheckedOutAmount conns uid
            bid <- checkIfBookInLibrary conns csISBN
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                INSERT INTO account_book (user_id, book_id, check_date, end_date)
                VALUES (?, ?, now(), now() + interval '14 days')
                RETURNING book_id, check_date, end_date
                |] (uid, bid)
            case rows of
                []           -> throwError db500Err
                (bid, coDate, eDate) : _ -> return $ CheckedOutBook bid postISBN coDate eDate 14
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db500Err = err500 { errBody = "Failed to create book" }

checkIfBookInLibrary :: Pool Connection -> String -> Handler Int
checkIfBookInLibrary conns csISBN = do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT book_id FROM book
                WHERE ISBN = ? AND book_id NOT IN (SELECT book_id FROM account_book)
                LIMIT 1
                |] (Only csISBN)
            case rows of
                []           -> throwError (err404 { errBody = "There is no such book currently in library" })
                Only bid : _ -> return bid

checkUsersOverdued :: Pool Connection -> Int -> Handler ()
checkUsersOverdued conns uid = do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT COUNT(*) FROM account_book
                WHERE user_id = ? AND date_trunc('day', now()) > date_trunc('day', account_book.end_date)
                |] (Only uid)
            case rows of
                []           -> return ()
                Only (0 :: Int) : _ -> return ()
                Only (_ :: Int) : _ -> throwError (err412 { errBody = "You have overdued book" })

checkCheckedOutAmount :: Pool Connection -> Int -> Handler ()
checkCheckedOutAmount conns uid = do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT COUNT(*) FROM account_book
                WHERE user_id = ?
                |] (Only uid)
            case rows of
                []           -> return ()
                Only count : _ -> when (count >= (3 :: Int)) $ throwError (err412 { errBody = "You have too many checked out books" })

returnBook :: Pool Connection -> AuthUser -> PostBook -> Handler Int
returnBook conns au (PostBook postISBN) = case postISBN of
    WrongISBN -> throwError wrongISBN400Err
    _ -> do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                WITH d AS (SELECT account_book.book_id FROM account_book
                           INNER JOIN book ON account_book.book_id = book.book_id
                           WHERE account_book.user_id = ? AND book.ISBN = ?
                           ORDER BY end_date
                           LIMIT 1)
                DELETE FROM account_book
                WHERE account_book.user_id = ? AND account_book.book_id IN (SELECT * FROM d)
                RETURNING account_book.book_id
                |] (unAuthUser au, isbnToString postISBN, unAuthUser au)
            case rows of
                []           -> throwError db404Err
                Only bid : _ -> return bid
    where wrongISBN400Err = err400 { errBody = "Wrong ISBN" }
          db404Err = err500 { errBody = "Can't return book" }

health :: Handler String
health = liftIO $ show . (round . (* 1000)) <$> getPOSIXTime

lookupAuthUser :: Pool Connection -> ByteString -> Handler AuthUser
lookupAuthUser conns key = do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT user_id FROM account
                WHERE token = ? AND role = 'user'
                |] (Only key)
            case rows of
                []           -> throwError (err403 { errBody = "Invalid Token" })
                Only uid : _ -> return $ AuthUser uid

lookupAuthLibrarian :: Pool Connection -> ByteString -> Handler AuthLibrarian
lookupAuthLibrarian conns key = do
            rows <- liftIO $ withResource conns $ \conn -> query conn [sql|
                SELECT user_id FROM account
                WHERE token = ? AND role = 'librarian'
                |] (Only key)
            case rows of
                []           -> throwError (err403 { errBody = "Invalid Token" })
                Only uid : _ -> return $ AuthLibrarian uid

authHandler :: Pool Connection -> (Pool Connection -> ByteString -> Handler a) -> AuthHandler Request a
authHandler conns check = mkAuthHandler handler
  where maybeToEither e = maybe (Left e) Right
        throw401 msg = throwError $ err401 { errBody = msg }
        handler req = either throw401 (check conns) $ maybeToEither "Missing Auth-Token header" $ lookup "Auth-Token" $ requestHeaders req

genAuthServerContext :: Pool Connection -> Context (AuthHandler Request AuthUser ': AuthHandler Request AuthLibrarian ': '[])
genAuthServerContext conns = authHandler conns lookupAuthUser :. authHandler conns lookupAuthLibrarian :. EmptyContext
