import Control.DeepSeq
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree as GP
import GraphBuilder
import InterpolationAlgorithms
import System.Environment
import Numeric.LinearAlgebra (size)
import System.Random (randomRIO)
import Utils
import Plots

main :: IO ()
main = do
  args <- getArgs
  file <- checkFile args
  let nbds = checkArgs args

  matrix' <- getReqMatrix file

  let (rsize,dims) = size matrix'

{-
The purpose of this algorithm is to find the Riemann normal
coordinates (a coordinate system permissible on Riemann manifolds) of
the tangent space at this point. This is built up by first finding the
K-nearest neighbors to this point, and then applying some linear
dimensionality reduction step to this collection of points (also
called a neighborhood). We perform principal component analysis,
however, other linear dimensionality reduction alogrithms are
permissible. The variable *BASEPOINT* is a randomly chosen point from
the dataset.
-}
  basePoint <- randomRIO (0,rsize-1)

  print $ "The base point is point " ++ show basePoint

{-
Find the M-nearest neighbors about each point (hereby referred to as
the "representative point" with respect to its neighbors) in the
dataset and determine the edges (their necessary existence and weight)
between each member of a neighorhood set and their shared
"representative" point as the Euclidean distance between the
representative point and every point in the neighborhood set.
-}

{-The variable *GCTX*, is a list of contexts for the graph. Essentially,
each context contains the nodes leaving and entering a given node. You
may wish to consult the FGL (functional Graph library) documentation
for more information. The variable *POINTS* is a list of the K nearest
neighbors from *BASEPOINT*, a randomnly chosen point in the
dataset. The variable *INDS* is a list of the indices of the points
contained in *POINTS*.
-}
  let (gctx,points,inds) = theContextsAndDists nbds basePoint matrix'

      -- Build the graph! Built from the contexts *GCTX*
      graph = force . foldr (joinContext . force) (empty :: GP.Gr () Double) $ gctx

      -- Deterimne the sets of shortest distances from points *POINTS*
      graphShortDists = shortDistMat rsize $ shortestDists inds graph

      {- This matrix is used in the interpolation step for finding the Riemann
          normal coordinates. Please refer to the paper in the repository for
          more information.
          -}
      res = estimateGrad (snd nbds) (dims-1) points
      -- The Riemann normal coordinates.
      res' = riemannNormCoord res graphShortDists

  -- If the graph is connected, then save the results of the analysis.
  if isConnected graph
     then plotAndSave2D' file rsize basePoint nbds res'
     else error "The graph is not connected."
