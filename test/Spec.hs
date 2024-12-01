import Test.HUnit

import Year2023
import Year2024

tests :: Test
tests = TestList [tests2023, tests2024]

main :: IO Counts
main = runTestTT tests
