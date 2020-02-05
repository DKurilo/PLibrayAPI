{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Lib                        (app, initConnectionPool)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app <$> initConnectionPool "") $ do
    describe "GET /api/v1/health" $ do
        it "responds with 200" $ do
            get "/api/v1/health" `shouldRespondWith` 200
