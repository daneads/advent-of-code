{-# LANGUAGE OverloadedStrings #-}

module Solutions.Year2023 (day1Part1, day1Part2, day2Part1, getIntsForLine, getValFromInts, solutions) where

import Data.Char (digitToInt, isDigit)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Lib

getIntsForLine :: String -> [Int]
getIntsForLine xs =
  let readInt x = (if isDigit x then Just $ digitToInt x else Nothing)
   in mapMaybe readInt xs

getValFromInts :: [Int] -> Int
getValFromInts xs = case length xs of
  1 -> read (show (head xs) ++ show (head xs)) :: Int
  _ -> read (show (head xs) ++ show (last xs)) :: Int

day1Part1 :: [String] -> String
day1Part1 xs =
  let allVals = fmap (getValFromInts . getIntsForLine) xs
   in show $ sum allVals

convertDigitStringsToInts :: String -> String
convertDigitStringsToInts s =
  let -- Some first/last chars are shared.
      -- E.g. "xtwone3four" -> x2134
      -- The below works around that problem by substituting first/last chars in addition to the digit
      stringsToDigits = [("one", "o1e"), ("two", "t2o"), ("three", "t3e"), ("four", "f4r"), ("five", "f5e"), ("six", "s6x"), ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e")]
      packedS = T.pack s
      runReplace (x, y) = T.replace x y
      replaced = foldl' (flip runReplace) packedS stringsToDigits
   in T.unpack replaced

day1Part2 :: [String] -> String
day1Part2 xs =
  let allVals = fmap (getValFromInts . getIntsForLine . convertDigitStringsToInts) xs
   in show $ sum allVals

parseColor :: T.Text -> (T.Text, Int)
parseColor s =
  let s' = T.words s
   in (last s', read . T.unpack $ head s')

maxColors :: HM.HashMap T.Text Int
maxColors = HM.fromList [("red", 12), ("green", 13), ("blue", 14)]

colorIsValid :: (T.Text, Int) -> Bool
colorIsValid (color, count) = count <= HM.findWithDefault 0 color maxColors

setIsValid :: HM.HashMap T.Text Int -> Bool
setIsValid set = all colorIsValid $ HM.toList set

parseSet :: T.Text -> HM.HashMap T.Text Int
parseSet s =
  let colorsRaw = T.splitOn ", " s
      colors = fmap parseColor colorsRaw
   in HM.fromList colors

parseGame :: String -> (Int, [HM.HashMap T.Text Int])
parseGame s =
  let s' = T.splitOn ": " (T.pack s)
      rawGame = T.words $ head s'
      gameId = read . T.unpack $ last rawGame
      rawSets = T.splitOn "; " $ last s'
      sets = fmap parseSet rawSets
   in (gameId, sets)

day2Part1 :: [String] -> String
day2Part1 xs =
  let games = fmap parseGame xs
      validGames = filter (\(_, game) -> all setIsValid game) games
      gameIdSum = sum $ fmap fst validGames
   in show gameIdSum

solutions :: [Solution]
solutions = [Solution 2023 1 1 day1Part1, Solution 2023 1 2 day1Part2, Solution 2023 2 1 day2Part1]
