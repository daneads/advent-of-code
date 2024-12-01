module Year2024 (tests2024) where
import Test.HUnit

import Solutions.Year2024

testsDayOne :: Test
testsDayOne = let
    day1Part1Solution = day1Part1 ["3 4", "4 3", "2 5", "1 3", "3 9", "3 3"]
    testday1Part1Sample = TestCase (assertEqual "Day 1 Part 1 Sample" "31" day1Part1Solution)
    day1Part2Solution = day1Part2 ["3 4", "4 3", "2 5", "1 3", "3 9", "3 3"]
    testday1Part2Sample = TestCase (assertEqual "Day 1 Part 2 Sample" "31" day1Part2Solution)
    in
        TestList [testday1Part1Sample, testday1Part2Sample]

tests2024 :: Test
tests2024 = TestList [testsDayOne]
