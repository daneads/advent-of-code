module Main (main) where

import Lib (Solution (solutionDay, solutionPart, solutionYear), runSolution)
import qualified Solutions.Year2023 as Y2023 (solutions)
import qualified Solutions.Year2024 as Y2024 (solutions)
import Text.Layout.Table

allSolutions :: [Solution]
allSolutions = Y2023.solutions ++ Y2024.solutions

getSolutionsTable :: [Solution] -> [String] -> String
getSolutionsTable solutions solutionStrs =
  let zippedSolutions = zip solutions solutionStrs
      cs = replicate 4 $ column expand left noAlign ellipsisCutMark
      h = titlesH ["Year", "Day", "Part", "Solution"]
      getSolutionRows (a, b) = rowG [show $ solutionYear a, show $ solutionDay a, show $ solutionPart a, b]
      dataRows = fmap getSolutionRows zippedSolutions
      t = columnHeaderTableS cs unicodeS h dataRows
   in tableString t

main :: IO ()
main = do
  solutionStrs <- traverse runSolution allSolutions
  let t = getSolutionsTable allSolutions solutionStrs
  putStrLn t
