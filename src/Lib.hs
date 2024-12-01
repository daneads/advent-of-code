module Lib
  ( Solution (Solution, solutionYear, solutionDay, solutionPart, solutionFunc),
    runSolution,
  )
where

import Paths_advent_of_code
import System.FilePath ((<.>), (</>))
import System.IO (readFile')

getFileData :: Int -> Int -> IO [String]
getFileData year day = do
  fpath <- getDataFileName $ "data" </> show year </> show day <.> "txt"
  contents <- readFile' fpath
  pure $ lines contents

data Solution = Solution
  { solutionYear :: Int,
    solutionDay :: Int,
    solutionPart :: Int,
    solutionFunc :: [String] -> String
  }

runSolution :: Solution -> IO String
runSolution s = do
  let year = solutionYear s
  let day = solutionDay s
  fileData <- getFileData year day
  let solution = solutionFunc s fileData
  pure solution
