module Lib
    ( Solution(Solution, solutionYear, solutionDay, solutionFunc), runSolution
    ) where

import System.FilePath ( (<.>), (</>) )
import System.IO (readFile')

import Paths_advent_of_code

getFileData :: Int -> Int -> IO [String]
getFileData year day = do
    fpath <- getDataFileName $ "data" </> show year </> show day <.> "txt"
    contents <- readFile' fpath
    pure $ lines contents

data Solution = Solution {
    solutionYear :: Int,
    solutionDay :: Int,
    solutionPart :: Int,
    solutionFunc :: [String] -> String
}

runSolution :: Solution -> IO String
runSolution s = do
    let year = solutionYear s
    let day = solutionDay s
    let part = solutionPart s
    fileData <- getFileData year day
    let solution = solutionFunc s fileData
    pure $ "Year " ++ show year ++ " Day " ++ show day ++ " Part " ++ show part ++ ": " ++ solution
