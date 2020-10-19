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

{-# LANGUAGE OverloadedStrings #-}

module BookStats where

import           Book

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Maybe
import           Data.List
import           Data.Function
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Control.Monad

data BooksStat = BooksStat {
    totalBooks :: Int
  , totalPages :: Int
  , ratedBest :: [Text]
  , mostReadAuthors :: [Text]
}

booksStat :: [Book] -> BooksStat
booksStat bks = BooksStat (length bks)
                          (sum $ map (fromMaybe 0 . nrPages) bks)
                          (getRatedBest bks)
                          (getMostReadAuthors bks)

getRatedBest :: [Book] -> [Text]
getRatedBest =
  map title . sortBy (compare `on` (myRating >=> \r -> return (5 - r)))

getMostReadAuthors :: [Book] -> [Text]
getMostReadAuthors =
  map fst
    . sortBy (compare `on` (negate . snd))
    . Map.toList
    . foldl' (\m b -> Map.insertWith (+) (author b) 1 m) Map.empty

groupByYear :: [Book] -> [(Text, [Book])]
groupByYear =
  Map.toList
    . foldl' (\m b -> Map.insertWith (++) (T.take 4 $ dateRead b) [b] m)
             Map.empty
