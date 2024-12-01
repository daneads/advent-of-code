module Main (main) where

import Lib ( Solution, runSolution )
import qualified Solutions.Year2023 as Y2023 (solutions)
import qualified Solutions.Year2024 as Y2024 (solutions)

allSolutions :: [Solution]
allSolutions = Y2023.solutions ++ Y2024.solutions

main :: IO ()
main = do
    solutionStrs <- traverse runSolution allSolutions
    mapM_ putStrLn solutionStrs
