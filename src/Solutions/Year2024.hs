module Solutions.Year2024 (solutions, day1Part1, day1Part2, day2Part1, day2Part2, day3Part1, day3Part2, findAllMul', splitbyDoDont, getPermutationslessOne) where

import Data.List (intercalate, sort)
import Data.List.Split (onSublist, split)
import Lib
import Text.Regex.TDFA ((=~))

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

findAllMul :: String -> [(Int, Int)]
findAllMul input =
  let regex = "mul\\(([0-9]+),([0-9]+)\\)" :: String
      matches = input =~ regex :: [[String]]
   in fmap (\[_whole, x, y] -> (read x, read y)) matches

day3Part1 :: [String] -> String
day3Part1 xs =
  let fullString = intercalate "" xs
      allMatches = findAllMul fullString
   in show . sum $ fmap (uncurry (*)) allMatches

findAllMul' :: [String] -> [(Int, Int)]
findAllMul' input = case input of
  [] -> []
  _ -> case head input of
    "don't()" ->
      let takeAfterDont = dropWhile (/= "do()") input
       in findAllMul' takeAfterDont
    _ ->
      let takeUntilDont = takeWhile (/= "don't()") input
          takeOnAfterDont = dropWhile (/= "don't()") input
       in findAllMul (concat takeUntilDont) ++ findAllMul' takeOnAfterDont

splitbyDoDont :: String -> [String]
splitbyDoDont s = concatMap (split (onSublist "don't()")) (split (onSublist "do()") s)

day3Part2 :: [String] -> String
day3Part2 xs =
  let fullString = intercalate "" xs
      allMatches = findAllMul' $ splitbyDoDont fullString
   in show . sum $ fmap (uncurry (*)) allMatches

solutions :: [Solution]
solutions =
  [ Solution 2024 1 1 day1Part1,
    Solution 2024 1 2 day1Part2,
    Solution 2024 2 1 day2Part1,
    Solution 2024 2 2 day2Part2,
    Solution 2024 3 1 day3Part1,
    Solution 2024 3 2 day3Part2
  ]
