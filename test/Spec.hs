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
import           Test.Hspec.Wai
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

spec :: Pool Connection -> Spec
spec conns = with (return (app conns)) $ do
    describe "GET /api/v1/health" $
        it "responds with 200" $
            get "/api/v1/health" `shouldRespondWith` 200
    describe "POST /api/v1/librarian/books" $ do
        it "responds with 403 when used anonymously" $
            post "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` 401
        it "responds with 403 when used as non-librarian" $
            postAs "2" "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` 403
        it "responds with 400 when wrong ISBN 13" $
            postAs "1" "/api/v1/librarian/books" "\"978-13-1662621-1\"" `shouldRespondWith` 400
        it "responds with 400 when wrong ISBN 10" $
            postAs "1" "/api/v1/librarian/books" "\"1484244696\"" `shouldRespondWith` 400
        it "responds with 400 when totally wrong ISBN" $
            postAs "1" "/api/v1/librarian/books" "\"The best book in the world\"" `shouldRespondWith` 400
        it "responds with created book" $ do
            let book = "{\"isbn\":\"9781316626221\",\"bookId\":\"4\"}"
            postAs "1" "/api/v1/librarian/books" "\"978-13-1662622-1\"" `shouldRespondWith` book {matchStatus = 201}
