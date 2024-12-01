{-# LANGUAGE OverloadedStrings #-}
module Solutions.Year2023 (day1Part1, day1Part2, getIntsForLine, getValFromInts, solutions) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import Lib
import Data.List (foldl')

getIntsForLine :: String -> [Int]
getIntsForLine xs =
    let
        readInt x = (if isDigit x then Just $ digitToInt x else Nothing)
    in
        mapMaybe readInt xs

getValFromInts :: [Int] -> Int
getValFromInts xs = case length xs of
    1 -> read (show (head xs) ++ show (head xs)) :: Int
    _ -> read (show (head xs) ++ show (last xs)) :: Int

day1Part1 :: [String] -> String
day1Part1 xs = let
    allVals = fmap (getValFromInts . getIntsForLine) xs
    in
        show $ sum allVals

convertDigitStringsToInts :: String -> String
convertDigitStringsToInts s = let
    stringsToDigits = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
    packedS = T.pack s
    runReplace (x, y) = T.replace x y
    replaced = foldl' (flip runReplace) packedS stringsToDigits
    in T.unpack replaced

day1Part2 :: [String] -> String
day1Part2 xs = let
    allVals = fmap (getValFromInts . getIntsForLine . convertDigitStringsToInts) xs
    in
        show $ sum allVals

solutions :: [Solution]
solutions = [Solution 2023 1 1 day1Part1, Solution 2023 1 1 day1Part2]
