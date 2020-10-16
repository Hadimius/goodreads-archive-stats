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

