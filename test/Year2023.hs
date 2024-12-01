module Year2023 (tests2023) where
import Test.HUnit

import Solutions.Year2023

testsDayOne :: Test
testsDayOne = let
    testDayOneGetIntsForLineOneInt = TestCase (assertEqual "getIntsForLine one int" [1] (getIntsForLine "a1bc"))
    testDayOneGetIntsForLineTwoInts = TestCase (assertEqual "getIntsForLine two ints" [1, 2] (getIntsForLine "1abc2"))
    day1Part1Solution = day1Part1 ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
    testday1Part1Sample = TestCase (assertEqual "Day 1 Part 1 Sample" "142" day1Part1Solution)
    day1Part2Solution = day1Part2 ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]
    testday1Part2Sample = TestCase (assertEqual "Day 1 Part 2 Sample" "281" day1Part2Solution)
    in
        TestList [testDayOneGetIntsForLineOneInt, testDayOneGetIntsForLineTwoInts, testday1Part1Sample, testday1Part2Sample]

tests2023 :: Test
tests2023 = TestList [testsDayOne]
