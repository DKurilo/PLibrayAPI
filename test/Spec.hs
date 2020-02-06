{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.ByteString            (ByteString)
import           Data.Pool                  (Pool (..))
import           Database.PostgreSQL.Simple (Connection)
import           Lib                        (app, getConnectionString,
                                             initConnectionPool)
import           Network.HTTP.Types.Method
import           Network.Wai.Test           hiding (request)
import           System.Environment         (lookupEnv)
import           Test.Hspec
import           Test.Hspec.Wai             hiding (delete)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = do
    connStr <- getConnectionString
    pool <- initConnectionPool connStr
    hspec (spec pool)

actAs token method path = request method path [("Auth-Token", token), ("Content-Type", "application/json")]

getAs token path = actAs token methodGet path ""

postAs token = actAs token methodPost

deleteAs token = actAs token methodDelete

delete path = request methodDelete path [("Content-Type", "application/json")]

spec :: Pool Connection -> Spec
spec conns = with (return (app conns)) $ do
    describe "GET /api/v1/health" $
        it "responds with 200" $
            get "/api/v1/health" `shouldRespondWith` 200
    describe "POST /api/v1/librarian/books" $ do
        it "responds with 401 when used anonymously" $
            post "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` 401
        it "responds with 403 when used as non-librarian" $
            postAs "2" "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` 403
        it "responds with 400 when wrong ISBN 13" $
            postAs "1" "/api/v1/librarian/books" "\"978-13-1662621-1\"" `shouldRespondWith` 400
        it "responds with 400 when wrong ISBN 10" $
            postAs "1" "/api/v1/librarian/books" "\"1484244696\"" `shouldRespondWith` 400
        it "responds with 400 when totally wrong ISBN" $
            postAs "1" "/api/v1/librarian/books" "\"The best book in the world\"" `shouldRespondWith` 400
        it "responds with created book when ISBN 13" $ do
            let book = "{\"isbn\":\"9781316626221\",\"bookId\":\"4\"}"
            postAs "1" "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` book {matchStatus = 201}
        it "responds with created book when ISBN 10" $ do
            let book = "{\"isbn\":\"9780262510875\",\"bookId\":\"5\"}"
            postAs "1" "/api/v1/librarian/books" "\"0-26251087-1\"" `shouldRespondWith` book {matchStatus = 201}
    describe "GET /api/v1/librarian/books/overdue" $ do
        it "responds with 401 when used anonymously" $
            get "/api/v1/librarian/books/overdue" `shouldRespondWith` 401
        it "responds with 403 when used as non-librarian" $
            getAs "2" "/api/v1/librarian/books/overdue" `shouldRespondWith` 403
        it "responds with list of overdued books" $ do
            let books = "[{\"daysOverdue\":\"16\",\"isbn\":\"9780441013593\",\"user\":\"2\",\"bookId\":\"1\"},{\"daysOverdue\":\"6\",\"isbn\":\"9780738811512\",\"user\":\"4\",\"bookId\":\"2\"}]"
            getAs "1" "/api/v1/librarian/books/overdue" `shouldRespondWith` books {matchStatus = 200}
    describe "Need more boks in Library" $
        it "just works" $ do
            postAs "1" "/api/v1/librarian/books" "\"9780321751041\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"9780321751041\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"0441013597\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"0131429019\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"9781449335908\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"9780521663502\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"0486411478\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"9781782834427\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"0-201-83595-9\"" `shouldRespondWith` 201
            postAs "1" "/api/v1/librarian/books" "\"978-0-13-595705-9\"" `shouldRespondWith` 201
    describe "DELETE /api/v1/librarian/books" $ do
        it "responds with 401 when used anonymously" $
            delete "/api/v1/librarian/books" "\"9780321751041\"" `shouldRespondWith` 401
        it "responds with 403 when used as non-librarian" $
            deleteAs "2" "/api/v1/librarian/books" "\"9780321751041\"" `shouldRespondWith` 403
        it "responds with list of overdued books" $
            deleteAs "1" "/api/v1/librarian/books" "\"9780321751041\"" `shouldRespondWith` "6" {matchStatus = 202}
        it "should delete first non-checked out book" $ do
            postAs "1" "/api/v1/librarian/books" "\"0262510871\"" `shouldRespondWith` 201
            deleteAs "1" "/api/v1/librarian/books" "\"0-26251087-1\"" `shouldRespondWith` "5" {matchStatus = 202}
        it "responds with 404 when no free books" $ do
            deleteAs "1" "/api/v1/librarian/books" "\"0-26251087-1\"" `shouldRespondWith` "16" {matchStatus = 202}
            deleteAs "1" "/api/v1/librarian/books" "\"0-26251087-1\"" `shouldRespondWith` 404
        it "responds with 404 when book is not in library" $
            deleteAs "1" "/api/v1/librarian/books" "\"978-0-262-03384-8\"" `shouldRespondWith` 404
