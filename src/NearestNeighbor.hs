module NearestNeighbor
       ( selectK
       , selectKtest
       , selectKRepa
       ) where

import           Control.Monad (join)
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

selectK
  :: V.Vector (U.Vector Double)
  -> Int -> Int -> Int
  -> ([(Double,Int)],Int,(),[(Double,Int)])
selectK vec len numK pt = if numK > 1
                          then neighbors
                          else error "Needs moar neighbors!"
  where
    neighbors = mkContexts . U.take numK . vsort $ U.map combLazy indices
    combLazy = metricSpaceDistBuild vec euclidDist pt
    indices = (U.++) (U.enumFromN 0 (pt-1)) (U.enumFromN (pt+1) (len-1-pt))
    mkContexts = (\a -> (a,pt,(),a)) . U.toList

selectKRepa
  :: V.Vector (U.Vector Double)
  -> Int -> Int -> Int
  -> (U.Vector Double, V.Vector (U.Vector Double),U.Vector Int)
selectKRepa vec len numK pt = if numK > 1
                          then (val,points',inds')
                          else error "Needs moar neighbors!"
  where
    neighbors = U.map (uncurry $ flip (,)) . U.take numK . vsort $ U.map combLazy indices
    combLazy = metricSpaceDistBuild vec euclidDist pt
    indices = (U.++) (U.enumFromN 0 (pt-1)) (U.enumFromN (pt+1) (len-1-pt))
    points' = U.foldr (\x acc -> V.snoc acc $ V.unsafeIndex vec $ fst x) V.empty neighbors
    inds' = U.map fst neighbors
    val = U.accumulate (\_ b -> b) dumbVector neighbors
    highnum = 1/0 :: Double
    dumbVector = (if pt == 0
                  then U.empty
                  else U.replicate pt highnum)
                 U.++
                 U.singleton 0
                 U.++
                 U.replicate (len-1-pt) highnum


selectKtest
  :: V.Vector (U.Vector Double)
  -> Int -> Int -> Int
  -> IntMap Double
selectKtest vec len numK pt = if numK > 1
                          then neighbors
                          else error "Needs moar neighbors!"
  where
    neighbors = mkMap . U.toList . U.take numK . vsort $ U.map combLazy indices
    combLazy = metricSpaceDistBuild vec euclidDist pt
    indices = (U.++) (U.enumFromN 0 (pt-1)) (U.enumFromN (pt+1) (len-1-pt))
    mkMap = fixmap . createMap
    createMap = IntMap.mapKeys (+1) . IntMap.fromList . map (uncurry $ flip (,))
    fixmap = id -- Work on this part later. You will need to make sure that the IntMap represents an undirected graph. (It is currently a directed graph.)

vsort :: U.Vector (Double,Int) -> U.Vector (Double,Int)
vsort v = runST $ do m <- G.unsafeThaw v
                     VA.sortBy (\x y -> if fst x == fst y then EQ else if fst x > fst y then GT else LT) m
                     G.unsafeFreeze m

metricSpaceDistBuild
  :: V.Vector (U.Vector Double)
  -> (U.Vector Double -> U.Vector Double -> Double)
  -> Int
  -> Int
  -> (Double,Int)
metricSpaceDistBuild vec symFunc idx1 idx2 = (dist' idx1 idx2,idx2)
  where
    dist' ind1 ind2 = if ind1 < ind2
                      then symFunc
                           (V.unsafeIndex vec ind1) (V.unsafeIndex vec ind2)
                      else symFunc
                           (V.unsafeIndex vec ind2) (V.unsafeIndex vec ind1)

euclidDist ::  U.Vector Double -> U.Vector Double -> Double
euclidDist xs ys = norm' xs + norm' ys - U.sum (dot' xs ys)
  where
    norm' = U.sum . join dot'
    dot' = U.zipWith (*)
