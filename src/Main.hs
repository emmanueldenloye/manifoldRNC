import DenseGraph
import InterpolationAlgorithms
import NearestNeighbor
import Plots
import System.Environment
import System.Random
import Utils

main :: IO ()
main = do
  args <- getArgs
  file <- checkFile args
  matrix <- getReqMatrix file

  let nbds = checkArgs args
      dataset = convertImages matrix
      len = getLength matrix
      f = selectK dataset len

  basePoint <- randomRIO (0,len :: Int) :: IO Int
  let (mat, points, inds) = buildShortestPaths f nbds basePoint

  res <- getDerivatives' points inds <$> getConnected mat
  plotAndSave2D' file len res basePoint
