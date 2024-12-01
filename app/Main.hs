module Main (main) where

import Lib (Solution (solutionDay, solutionPart, solutionYear), runSolution)
import qualified Solutions.Year2023 as Y2023 (solutions)
import qualified Solutions.Year2024 as Y2024 (solutions)
import Text.Layout.Table

allSolutions :: [Solution]
allSolutions = Y2023.solutions ++ Y2024.solutions

getSolutionsTable :: [Solution] -> IO String
getSolutionsTable solutions = do
  solutionStrs <- traverse runSolution allSolutions
  let zippedSolutions = zip solutions solutionStrs
  let cs = replicate 4 $ column expand left noAlign ellipsisCutMark
  let h = titlesH ["Year", "Day", "Part", "Solution"]
  let getSolutionRows (a, b) = rowG [show $ solutionYear a, show $ solutionDay a, show $ solutionPart a, b]
  let dataRows = fmap getSolutionRows zippedSolutions
  let t = columnHeaderTableS cs unicodeS h dataRows
  pure $ tableString t

main :: IO ()
main = do
  t <- getSolutionsTable allSolutions
  putStrLn t
