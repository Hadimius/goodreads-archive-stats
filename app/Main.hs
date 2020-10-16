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
