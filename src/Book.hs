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


import Prelude hiding (lookup)
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Data.Csv                       
import qualified Data.HashMap.Strict           as H
import qualified Data.ByteString               as BS
import           Data.Functor                   ( (<&>) )
import           Data.Function                  ( (&) )

import           Text.Read                      ( readMaybe )

import           GHC.Generics                   ( Generic )

data Book = Book
 { bookId :: Maybe Integer
  , title :: Text
  , author :: Text
  , authorLF :: Text
  , additionalAuthors :: Maybe Text
  , isbn :: Maybe Text
  , isbn13 :: Maybe Text
  , myRating :: Maybe Int
  , avgRating :: Maybe Double
  , publisher :: Maybe Text
  , binding :: Maybe Text
  , nrPages :: Maybe Int
  , yearPublished :: Maybe Int
  , originalYearPublished :: Maybe Int
  , dateRead :: Maybe Text
  , dateAdded :: Maybe Text
  , bookshelves :: Maybe Text
  , bookshelvesWithPos :: Maybe Text
  , exclusiveShelf :: Maybe Text
  , myReview :: Maybe Text
  , spoiler :: Maybe Text
  , privateNotes :: Maybe Text
  , readCount :: Maybe Int
  , recommendedFor :: Maybe Text
  , recommendedBy :: Maybe Text
  , ownedCopies  :: Maybe Int
  , originalPurchaseDate :: Maybe Text
  , originalPurchaseLocation :: Maybe Text
  , condition :: Maybe Text
  , conditionDescription :: Maybe Text
  , bcid :: Maybe Text
} deriving (Show, Generic)

--unlessEquals :: Eq a => a -> Maybe a -> Maybe a
--unlessEquals x ma = ma >>= if ma == Just x then Nothing else ma


--lookupTextKey :: BS.ByteString -> NamedRecord -> Parser (Maybe Text)
--lookupTextKey k m = H.lookup k m <&> decodeUtf8 & unlessEquals ""

lookupNumKey :: Read a => BS.ByteString -> NamedRecord -> Maybe a
lookupNumKey k m = H.lookup k m <&> (T.unpack . decodeUtf8) >>= readMaybe

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
      <*> m
      H.! "Author l-f"
      <*> lookupTextKey "Additional Authors" m
      <*> lookupTextKey "ISBN"               m
      <*> lookupTextKey "ISBN13"             m
      <*> lookupNumKey "My Rating"
      <*> lookupNumKey "Average Rating"
      <*> lookupTextKey "Publisher" m
      <*> lookupTextKey "Binding"   m
      <*> lookupNumKey "Number of Pages"
      <*> lookupNumKey "Year Published"
      <*> lookupNumKey "Original Publication Year"
      <*> lookupTextKey "Date Read"                  m
      <*> lookupTextKey "Date Added"                 m
      <*> lookupTextKey "Bookshelves"                m
      <*> lookupTextKey "Bookshelves with positions" m
      <*> lookupTextKey "Exclusive Shelf"            m
      <*> lookupTextKey "My Review"                  m
      <*> lookupTextKey "Spoiler"                    m
      <*> lookupTextKey "Private Notes"              m
      <*> lookupNumKey "Read Count"
      <*> lookupTextKey "Recommended For" m
      <*> lookupTextKey "Recommended By"  m
      <*> lookupNumKey "Owned Copies"
      <*> lookupTextKey "Original Purchase Date"     m
      <*> lookupTextKey "Original Purchase Location" m
      <*> lookupTextKey "Condition"                  m
      <*> lookupTextKey "Condition Description"      m
      <*> lookupTextKey "BCID"                       m

