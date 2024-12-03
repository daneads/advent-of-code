import qualified System.Exit as Exit
import Test.HUnit
import Year2023
import Year2024

tests :: Test
tests = TestList [tests2023, tests2024]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
