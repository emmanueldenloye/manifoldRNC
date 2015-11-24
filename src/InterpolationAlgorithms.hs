{-#LANGUAGE BangPatterns#-}
module InterpolationAlgorithms
  (gradInterpolation,getQuadCoeffs)
  where

import           Control.Monad (join)
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Internal.RootPath
import qualified Data.Graph.Inductive.Internal.Heap as HP
import qualified Data.Graph.Inductive.PatriciaTree as GP
import qualified Data.List as LS
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import           Debug.Trace
import qualified Numeric.LinearAlgebra as L
import           PCA

-- import           Control.ParallelS.trategies

-- I am going to need to use inplace
-- or parallel code to make this work in a satisfactory time

-- getQuadCoeffs :: forall t (c :: * -> *).
--                        (Num (L.Vector t), L.Field t, L.LSDiv c) =>
--                        c t -> [L.Vector t] -> c t
-- getQuadCoeffs ptdists projs = (L.<\>) (buildRect projs) ptdists
--   where
--     buildRect = L.fromRows . map rowBuilder
--     rowBuilder rw = L.vjoin[1,rw,L.flatten . join L.outer $ rw]

getQuadCoeffs :: L.Matrix L.R -> [[L.R]] -> L.Matrix L.R
getQuadCoeffs ptdists projs = let v1 = ptdists
                                  v2 = buildRect projs
                              in (L.<\>) v2 v1
  where
    buildRect = L.fromRows . map rowBuilder
    rowBuilder rw = L.vjoin[1,L.vector rw,L.flatten $ L.vector rw `L.outer` L.vector rw]

-- gradInterpolation :: (Real b) => GP.Gr a1 b -> V.Vector (U.Vector Double) -> Int -> (t3 -> ([(a, Int)], t, t1, t2)) -> t3 -> [b]
gradInterpolation graph points gradlen nearest = dists
    -- L.size . L.fromRows . dists
  where
    projections pt = let vals = nbds pt
                         themat = getCov gradlen (L.fromRows vals)
                      in foldr ((:) . L.toList . (L.#>) themat) [] vals
                       -- map (L.<# themat) vals
    dists pt = case nearest pt of
                 (val,_,_,_) -> let valsnd = map snd val
                                    rest = (LS.\\) [0..V.length points-1] valsnd
                                    -- preheap = map (\y -> (0,LP [(y,0)])) valsnd
                                    -- shortFunc x = getDistance x $ dijkstra (HP.build preheap) graph
                                in map (\x -> spLength 0 x graph) rest
                                  -- HP.prettyHeap $ HP.build preheap
                                  -- map (\x -> spLength x (head rest) graph) (take 800 valsnd)
                                  -- map (\x -> let v = head rest
                                  --                 in spLength v x graph) valsnd

                                  -- trace "evalled" $ map (\x -> spLength (head rest) x graph) valsnd
                   -- trace "begin" $ map ( tail . flip spTree graph . snd) val
    -- foldr (trace "datcons" . (:) . tail . flip spTree graph . snd) [] val
                   -- map (trace "wtf"  . map (snd . head . unLPath) . tail . flip spTree graph . snd)  val
    -- quadCoeffs pt = getQuadCoeffs
    --                 (L.fromRows  $ dists pt)
    --                 (projections pt)
    nbds = Prelude.map (S.fromList . U.toList . V.unsafeIndex points) . nIndices
    nIndices pt = case nearest pt of
                    (val,_,_,_) -> map snd val
    -- nodeGrad pt = let ws = L.toColumns $ quadCoeffs pt L.?? (L.Range 1 1 gradlen ,L.All) /(-2)
    --               in map L.toList ws

-- {-#LANGUAGE FlexibleContexts#-}
-- {-#LANGUAGE RankNTypes#-}
-- {-#LANGUAGE KindSignatures#-}
