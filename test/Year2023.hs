module Year2023 (tests2023) where

import Solutions.Year2023
import Test.HUnit

testsDayOne :: Test
testsDayOne =
  let testDayOneGetIntsForLineOneInt = TestCase (assertEqual "getIntsForLine one int" [1] (getIntsForLine "a1bc"))
      testDayOneGetIntsForLineTwoInts = TestCase (assertEqual "getIntsForLine two ints" [1, 2] (getIntsForLine "1abc2"))
      day1Part1Solution = day1Part1 ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
      testday1Part1Sample = TestCase (assertEqual "Day 1 Part 1 Sample" "142" day1Part1Solution)
      day1Part2Solution = day1Part2 ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]
      testday1Part2Sample = TestCase (assertEqual "Day 1 Part 2 Sample" "281" day1Part2Solution)
   in TestList [testDayOneGetIntsForLineOneInt, testDayOneGetIntsForLineTwoInts, testday1Part1Sample, testday1Part2Sample]

testsDayTwo :: Test
testsDayTwo =
  let day2Part1Solution =
        day2Part1
          [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
          ]
      testday2Part1Sample = TestCase (assertEqual "Day 2 Part 1 Sample" "8" day2Part1Solution)
   in TestList [testday2Part1Sample]

tests2023 :: Test
tests2023 = TestList [testsDayOne, testsDayTwo]
