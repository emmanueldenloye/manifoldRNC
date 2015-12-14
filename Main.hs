import Control.DeepSeq
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree as GP
import GraphBuilder
import InterpolationAlgorithms
import Numeric.LinearAlgebra (size)
import System.Environment
import System.FilePath
import System.Random (randomRIO)
import Utils

main :: IO ()
main = do
  args <- getArgs
  file <- checkFile args
  let nbds = checkArgs args

  matrix' <- getReqMatrix file

  let (rsize,dims) = size matrix'

  basePoint <- randomRIO (0,rsize-1)

  let (gctx,points,inds) = theContextsAndDists nbds basePoint matrix'
      gradlen = 2 -- for testing, I will ask the user for this later!
      graph = force . foldr (joinContext . force) (empty :: GP.Gr () Double) $ gctx
      graphShortDists = shortDistMat rsize . shortestDists inds $ graph

  saveRes nbds (takeBaseName file) basePoint . estimateGrad dims (snd nbds) gradlen points $ graphShortDists
