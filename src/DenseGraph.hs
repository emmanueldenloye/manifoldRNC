{-#LANGUAGE BangPatterns#-}
{-#OPTIONS_GHC -Odph -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3#-}
module DenseGraph where

import Data.Array.Repa
import Data.Functor.Identity (runIdentity)
import qualified Data.Vector.Unboxed as U
import  qualified Numeric.LinearAlgebra as L
import qualified Data.Vector.Storable as S (any)
import qualified Data.Vector as V
import Data.List (sort)

type Weight = Double
type Graph r = Array r DIM2 Weight

buildShortestPaths
  :: (Int -> Int -> (U.Vector Double, V.Vector (U.Vector Double),U.Vector Int))
  -> (Int,Int)
  -> Int
  -> (L.Matrix Double, V.Vector (U.Vector Double), [Int])
buildShortestPaths nbdfunc nbds basePoint = res
  where
    (len,points,inds) = (\(a,b,c) -> (U.length a,b,sort $ U.toList c))
                          . nbdfunc (snd nbds) $ basePoint
    sp = shortestPaths repa
    repa = fromUnboxed (Z :. len :. len)
            . U.concatMap ((\(a,_,_) -> a) . nbdfunc (fst nbds))
            $ nodeEnum :: Graph U
    nodeEnum = U.enumFromN (0 :: Int) len
    res = (L.matrix len . Data.Array.Repa.toList $ sp, points, inds)

getConnected :: L.Matrix Double -> IO (L.Matrix Double)
getConnected v = return $ let row' = extract' v
                          in if anyInf row'
                              then error "The graph is not connected."
                              else v
  where
    extract' = flip (L.??) (L.Take 1,L.All) :: L.Matrix Double -> L.Matrix Double
    anyInf = S.any ((1/0 :: Double) ==) . L.flatten

shortestPaths :: Graph U -> Graph U
shortestPaths g0 = runIdentity $ go g0 0
  where
    Z :. _ :. n = extent g0
    go !g !k | k == n    = return g
             | otherwise = do
                 g' <- computeP (fromFunction (Z:.n:.n) sp)
                 go g' (k+1)
     where
       sp (Z:.i:.j) = min
                      (g Data.Array.Repa.! (Z:.i:.j))
                      (g Data.Array.Repa.! (Z:.i:.k)
                       + g Data.Array.Repa.! (Z:.k:.j))
