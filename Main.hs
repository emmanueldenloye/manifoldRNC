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
      dataset = convertDataset matrix
      rowsize = rowSize matrix
      f = selectK dataset rowsize

  basePoint <- randomRIO (0,rowsize-1 :: Int) :: IO Int
  let (mat, points, inds) = buildShortestPaths f nbds basePoint

  res <- getDerivatives' points inds <$> getConnected mat
  plotAndSave2D' file rowsize res basePoint
