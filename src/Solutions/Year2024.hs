module Solutions.Year2024 (solutions, day1Part1, day1Part2, day2Part1, day2Part2, getPermutationslessOne) where

import Data.List (sort)
import Lib

getInts :: String -> [Int]
getInts line = (read <$> words line) :: [Int]

getLocationLists :: [String] -> ([Int], [Int])
getLocationLists xs =
  let ints = fmap getInts xs
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

reportIsSafe :: [Int] -> Bool
reportIsSafe xs =
  let diffs = zipWith (-) (init xs) (tail xs)
      allAscOrDesc = all (> 0) diffs || all (< 0) diffs
      stepsInRange = all ((\y -> 1 <= y && y <= 3) . abs) diffs
   in allAscOrDesc && stepsInRange

day2Part1 :: [String] -> String
day2Part1 xs = show . length $ filter (reportIsSafe . getInts) xs

removeElem :: Int -> [a] -> [a]
removeElem _ [] = []
removeElem 0 (_ : xs) = xs
removeElem n (x : xs) = x : removeElem (n - 1) xs

getPermutationslessOne :: [a] -> [[a]]
getPermutationslessOne xs = fmap (`removeElem` xs) [0 .. length xs - 1]

reportIsSafe' :: [Int] -> Bool
reportIsSafe' xs =
  let allPermutations = getPermutationslessOne xs
   in reportIsSafe xs || any reportIsSafe allPermutations

day2Part2 :: [String] -> String
day2Part2 xs = show . length $ filter (reportIsSafe' . getInts) xs

solutions :: [Solution]
solutions =
  [ Solution 2024 1 1 day1Part1,
    Solution 2024 1 2 day1Part2,
    Solution 2024 2 1 day2Part1,
    Solution 2024 2 2 day2Part2
  ]
