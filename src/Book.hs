{-# LANGUAGE DeriveGeneric #-}

module Book
  ( Book(..)
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Csv

import           GHC.Generics

data Book = Book
 { bookId :: Maybe Integer
  , title :: Text
  , author :: Text
  , authorLF :: Text
  , additionalAuthors :: Text
  , isbn :: Text
  , isbn13 :: Text
  , rating :: Maybe Int
  , avgRating :: Maybe Double
  , publisher :: Text
  , binding :: Text
  , nrPages :: Maybe Int
  , yearPublished :: Maybe Int
  , originalYearPublished :: Maybe Int
  , dateRead :: Text
  , dateAdded :: Text
  , exclShelf :: Text
  , review :: Text
} deriving (Show, Generic)

instance FromRecord Book

