module Solutions.Year2024 (solutions, day1Part1, day1Part2) where

import Data.List (sort)
import Lib

getLocationLists :: [String] -> ([Int], [Int])
getLocationLists xs =
  let getInts line = (read <$> words line) :: [Int]
      ints = fmap getInts xs
      list1 = sort $ fmap head ints
      list2 = sort $ fmap last ints
   in (list1, list2)

day1Part1 :: [String] -> String
day1Part1 xs =
  let (list1, list2) = getLocationLists xs
      diffs = zipWith (\a b -> abs $ a - b) list1 list2
   in show $ sum diffs

day1Part2 :: [String] -> String
day1Part2 xs =
  let (list1, list2) = getLocationLists xs
      rowSimilarityScore x = x * length (filter (== x) list2)
      similarityScores = fmap rowSimilarityScore list1
   in show $ sum similarityScores

solutions :: [Solution]
solutions = [Solution 2024 1 1 day1Part1, Solution 2024 1 2 day1Part2]
