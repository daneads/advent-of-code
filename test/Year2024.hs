module Year2024 (tests2024) where

import Solutions.Year2024
import Test.HUnit

testsDayOne :: Test
testsDayOne =
  let part1Solution = day1Part1 ["3 4", "4 3", "2 5", "1 3", "3 9", "3 3"]
      testPart1Sample = TestCase (assertEqual "Day 1 Part 1 Sample" "11" part1Solution)
      part2Solution = day1Part2 ["3 4", "4 3", "2 5", "1 3", "3 9", "3 3"]
      testPart2Sample = TestCase (assertEqual "Day 1 Part 2 Sample" "31" part2Solution)
   in TestList [testPart1Sample, testPart2Sample]

testsDayTwo :: Test
testsDayTwo =
  let sampleData =
        [ "7 6 4 2 1",
          "1 2 7 8 9",
          "9 7 6 2 1",
          "1 3 2 4 5",
          "8 6 4 4 1",
          "1 3 6 7 9"
        ]
      testPart1Sample = TestCase (assertEqual "Day 2 Part 1 Sample" "2" $ day2Part1 sampleData)
      testPart2Sample = TestCase (assertEqual "Day 2 Part 2 Sample" "4" $ day2Part2 sampleData)
      testGetAllPermutations = TestCase (assertEqual "Day 2 Part 2 getAllPermutations" [[2, 3, 4], [1, 3, 4], [1, 2, 4], [1, 2, 3]] $ getPermutationslessOne [1, 2, 3, 4])
   in TestList [testPart1Sample, testPart2Sample, testGetAllPermutations]

tests2024 :: Test
tests2024 = TestList [testsDayOne, testsDayTwo]
