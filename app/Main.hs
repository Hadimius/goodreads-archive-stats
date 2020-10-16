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

module Main where

import           Book
import           Data.Csv
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as BS
import           Data.Maybe


sumPages :: V.Vector Book -> Int
sumPages = V.sum . V.map (fromMaybe 0 . nrPages)

main :: IO ()
main = do
  csvData <- BS.getContents
  case decode HasHeader csvData of
    Left  err -> putStrLn err
    Right v   -> do
      V.forM_ (V.filter (\b -> "2020" `T.isInfixOf` dateRead b) v)
        $ \book -> putStrLn $ T.unpack (title book) ++ show (nrPages book)
      print $ sumPages (V.filter (\b -> "2020" `T.isInfixOf` dateRead b) v)
