module InterpolationAlgorithms
  (gradInterpolation)
  where

import           Control.Monad (join)
import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Numeric.LinearAlgebra as L
import           PCA

getQuadCoeffs :: L.Matrix L.R -> [[L.R]] -> L.Matrix L.R
getQuadCoeffs ptdists projs = L.linearSolveLS (buildRect projs) ptdists
  where
    buildRect = L.fromRows . map rowBuilder
    rowBuilder rw = L.vjoin[1,L.vector rw,L.flatten . join L.outer . L.vector $ rw]

gradInterpolation
  :: GP.Gr () Double
  -> V.Vector (U.Vector Double)
  -> Int
  -> (Int -> ([(Double, Int)], Int, (),[(Double,Int)]))
  -> Int
  -> [[Double]]
gradInterpolation graph points gradlen nearest = nodeGrad
  where
    projections pt = let vals = nbds pt
                         themat = getCov gradlen (L.fromRows vals)
                     in foldr ((:) . L.toList . (L.#>) themat) [] vals
    dists pt = case nearest pt of
                 (val,_,_,_) -> map (L.vector . map (snd . head . unLPath) . tail . flip spTree graph . snd)  val
    quadCoeffs pt = getQuadCoeffs
                    (L.fromRows  $ dists pt)
                    (projections pt)
    nbds = Prelude.map (S.fromList . U.toList . V.unsafeIndex points) . nIndices
    nIndices pt = case nearest pt of
                    (val,_,_,_) -> map snd val
    nodeGrad pt = let ws = L.toColumns $ quadCoeffs pt L.?? (L.Range 1 1 gradlen ,L.All) /(-2)
                  in map L.toList ws
