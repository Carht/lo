module Lib
    ( gigabyteStr,
      toGigabytes,
    ) where

import System.Posix.Types (FileOffset)

kilobyte :: Integral a => a -> a
kilobyte bytes
  | bytes <= 1000 = bytes
  | otherwise = div bytes 1000

kilobyteStr :: (Show a, Integral a) => a -> String
kilobyteStr bytes
  | bytes <= 1000 = show bytes
  | otherwise = show (kilobyte bytes) <> "K"

megabyte :: Integral a => a -> a
megabyte bytes
  | bytes <= 1000000 = kilobyte bytes
  | otherwise = div (kilobyte bytes) 1000

megabyteStr :: (Show a, Integral a) => a -> String
megabyteStr bytes
  | bytes <= 1000000 = kilobyteStr bytes
  | otherwise = show (megabyte bytes) <> "M"

gigabyte :: Integral a => a -> a
gigabyte bytes
  | bytes <= 1000000000 = megabyte bytes
  | otherwise = div (megabyte bytes) 1000

gigabyteStr :: (Show a, Integral a) => a -> String
gigabyteStr bytes
  | bytes <= 1000000000 = megabyteStr bytes
  | otherwise = show (gigabyte bytes) <> "G"

toGigabytes :: (Show b, Integral b) => [(String, b)] -> [[String]]
toGigabytes [] = []
toGigabytes (x:xs) = [fst x, gigabyteStr . snd $ x] :  toGigabytes xs
