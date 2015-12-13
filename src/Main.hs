{-#LANGUAGE BangPatterns#-}

import           Control.Arrow
import           Control.DeepSeq
import           Data.Graph.Inductive
import           Data.Graph.Inductive.PatriciaTree as GP
import           Data.List (foldl',sortOn)
import qualified Data.Vector.Storable as S
import           GraphBuilder
import           InterpolationAlgorithms
import           Numeric.LinearAlgebra (size,rank,det,meanCov) -- for testing
import           Numeric.LinearAlgebra.Data
import           Numeric.LinearAlgebra.HMatrix
-- import           Plots
import           System.Environment
import           Utils

main :: IO ()
main = do
  args <- getArgs
  file <- checkFile args
  let nbds = checkArgs args

  matrix' <- getReqMatrix file

      -- graph' = foldr joinContext (empty :: GP.Gr () Double) . theContexts (fst nbds) $ matrix'
      -- sDists = map (`shortestDists` graph') [0..rsize-1 :: Int]

  -- print . (head &&& length) $ theContexts (fst nbds) matrix'
  let ectxs = theContextsAndDists nbds basePoint matrix'
      basePoint = 0 -- for testing
      graph = force $ foldr (joinContext . force) (empty :: GP.Gr () Double) $ fst ectxs
      graphShortDists = shortDistMat rsize . shortestDists rsize $ graph
      rsize = rowSize matrix'

  print . fst . eigSH . trustSym . snd . meanCov . snd $ ectxs

  print . size $ graphShortDists

    -- in if isConnected graph
    --     then print $ nodes graph
    --    else error "Continue later!"

  -- print $ checkSymRowsCols . shortDistMat rsize $ fst ectxs -- checkSymRowsCols used for debugging. It is in GraphBuilder.hs.
