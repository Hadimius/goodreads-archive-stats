-- Copyright (C) 2020 Juri Dispan
--
-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 59 Temple
-- Place, Suite 330, Boston, MA 02111-1307 USA

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Book
  ( Book(..)
  )
where

import           Prelude                 hiding ( lookup )
import           Data.Text                      ( Text )
import qualified Data.HashMap.Strict           as H
import           Data.Csv                       ( lookup
                                                , FromNamedRecord(..)
                                                , FromRecord
                                                )


import           GHC.Generics                   ( Generic )

data Book = Book
 { bookId :: Maybe Integer
  , title :: Text
  , author :: Text
  , authorLF :: Text
  , additionalAuthors :: Text
  , isbn :: Text
  , isbn13 :: Text
  , myRating :: Maybe Int
  , avgRating :: Maybe Double
  , publisher :: Text
  , binding :: Text
  , nrPages :: Maybe Int
  , yearPublished :: Maybe Int -- TODO Use proper date type
  , originalYearPublished :: Maybe Int -- TODO Use proper date type
  , dateRead :: Text -- TODO Use proper date type
  , dateAdded :: Text -- TODO Use proper date type
  , bookshelves :: Text
  , bookshelvesWithPos :: Text
  , exclusiveShelf :: Text -- TODO Use proper shelve type
  , myReview :: Text
  , spoiler :: Text -- TODO Is this a bool?
  , privateNotes :: Text
  , readCount :: Maybe Int
  , recommendedFor :: Text
  , recommendedBy :: Text
  , ownedCopies :: Maybe Int
  , originalPurchaseDate :: Text -- TODO Use proper date type
  , originalPurchaseLocation :: Text
  , condition :: Text
  , conditionDescription :: Text
  , bcid :: Text -- TODO What's a BCID?
} deriving (Show, Generic)

----
-- Deserialisation from CSV
----

instance FromRecord Book

-- deprecated until someone wants to use the decodeByName method:
instance FromNamedRecord Book where
  parseNamedRecord m
    | not
      $  ("Title" `H.member` m)
      && ("Author" `H.member` m)
      && ("Author l-f" `H.member` m)
    = fail "Title, Author or Authot l-f missing"
    | otherwise
    = Book
      <$> lookup m "Book Id"
      <*> lookup m "Title"
      <*> lookup m "Author"
      <*> lookup m "Author l-f"
      <*> lookup m "Additional Authors"
      <*> lookup m "ISBN"
      <*> lookup m "ISBN13"
      <*> lookup m "My Rating"
      <*> lookup m "Average Rating"
      <*> lookup m "Publisher"
      <*> lookup m "Binding"
      <*> lookup m "Number of Pages"
      <*> lookup m "Year Published"
      <*> lookup m "Original Publication Year"
      <*> lookup m "Date Read"
      <*> lookup m "Date Added"
      <*> lookup m "Bookshelves"
      <*> lookup m "Bookshelves with positions"
      <*> lookup m "Exclusive Shelf"
      <*> lookup m "My Review"
      <*> lookup m "Spoiler"
      <*> lookup m "Private Notes"
      <*> lookup m "Read Count"
      <*> lookup m "Recommended For"
      <*> lookup m "Recommended By"
      <*> lookup m "Owned Copies"
      <*> lookup m "Original Purchase Date"
      <*> lookup m "Original Purchase Location"
      <*> lookup m "Condition"
      <*> lookup m "Condition Description"
      <*> lookup m "BCID"
