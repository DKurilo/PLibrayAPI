{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Types
    ( DBConnectionString (..)
    , PutAccepted (..)
    , ISBN (..), isbnFromString, isbnToString
    , PostBook (..)
    , OverdueBook (..)
    , CheckedOutBook (..)
    , Book (..)
    , BookId (..)
    , API (..)
    , AuthUser (..)
    , AuthLibrarian (..)
    ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types                 (Parser)
import           Data.ByteString                  (ByteString)
import           Data.Char                        (isDigit)
import           Data.Text                        (Text, pack, unpack)
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time                        (LocalTime)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

type DBConnectionString = ByteString

data ISBN = WrongISBN | ISBN10 String | ISBN13 String deriving (Eq)

isbnFromString :: String -> ISBN
isbnFromString cs | l == 10 && checkSum10ISBN cs' = ISBN10 cs'
                  | l == 13 && checkSum13ISBN cs' = ISBN13 cs'
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

data Book = Book { bid   :: Int
                 , bisbn :: ISBN
                 }

instance ToJSON Book where
    toJSON b = object [ "bookId" .= (show . bid) b
                      , "isbn" .= (show . bisbn) b
                      ]

data OverdueBook = OverdueBook { obid   :: Int
                               , obisbn :: ISBN
                               , obdays :: Int
                               , obuser :: Int
                               }

instance ToJSON OverdueBook where
    toJSON ob = object [ "bookId" .= (show . obid) ob
                       , "isbn" .= (show . obisbn) ob
                       , "daysOverdue" .= (show . obdays) ob
                       , "user" .= (show . obuser) ob
                       ]

data CheckedOutBook = CheckedOutBook { cobid       :: Int
                                     , cobisbn     :: ISBN
                                     , cobcodate   :: LocalTime
                                     , cobedate    :: LocalTime
                                     , cobdaysLeft :: Int
                                     }

instance ToJSON CheckedOutBook where
    toJSON ob = object [ "bookId" .= (show . cobid) ob
                       , "isbn" .= (show . cobisbn) ob
                       , "checkoutDate" .= (show . cobcodate) ob
                       , "endDate" .= (show . cobedate) ob
                       , "daysLeft" .= (show . cobdaysLeft) ob
                       ]

type BookId = Int

newtype AuthUser = AuthUser { unAuthUser :: Int }
type instance AuthServerData (AuthProtect "user") = AuthUser
newtype AuthLibrarian = AuthLibrarian { unAuthLibrarian :: Int }
type instance AuthServerData (AuthProtect "librarian") = AuthLibrarian

type API = "api" :> "v1" :> "health" :> Get '[JSON] String
      :<|> "api" :> "v1" :> "librarian" :> "books" :> AuthProtect "librarian" :> ReqBody '[JSON] PostBook :> PostCreated '[JSON] Book
      :<|> "api" :> "v1" :> "librarian" :> "books" :> AuthProtect "librarian" :> ReqBody '[JSON] PostBook :> DeleteAccepted '[JSON] BookId
      :<|> "api" :> "v1" :> "librarian" :> "books" :> "overdue" :> AuthProtect "librarian" :> Get '[JSON] [OverdueBook]
      :<|> "api" :> "v1" :> "user" :> "books" :> AuthProtect "user" :> Get '[JSON] [CheckedOutBook]
      :<|> "api" :> "v1" :> "user" :> "books" :> AuthProtect "user" :> ReqBody '[JSON] PostBook :> PostCreated '[JSON] CheckedOutBook
      :<|> "api" :> "v1" :> "user" :> "books" :> AuthProtect "user" :> ReqBody '[JSON] PostBook :> DeleteAccepted '[JSON] BookId
