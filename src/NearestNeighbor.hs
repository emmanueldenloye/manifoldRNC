module NearestNeighbor
       ( selectK
       , combineHalves
       ) where

import           Data.List (sort)
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

selectK
  :: Int
  -> U.Vector Double
  -> V.Vector (U.Vector Double)
  -> Maybe (V.Vector (Int, U.Vector Double, Double))
selectK numK pt setx = if isJust ptIdx && numK >=1
                         then fmap (V.fromList . mapP) fsort
                         else Nothing
  where
    fsort = if isJust comb
               then Just . Prelude.take numK . sortV . clean $ comb
               else Nothing
    ptIdx = V.elemIndex pt setx
    comb  = combineHalves (fromJust ptIdx) $ simpleBuild setx
    mapP  = map (\(a,b) -> (b,(V.!) setx b,a))
    clean = U.map (uncurry $ flip (,)) . U.indexed . fromJust
    sortV = sort . U.toList

simpleBuild :: V.Vector (U.Vector Double) -> V.Vector (U.Vector Double)
simpleBuild = V.unfoldr unfoldFunc
  where
    unfoldFunc x =  if V.null . V.tail $ x
                     then Nothing
                     else Just $ distVecPair x
    distVecPair xs = (,) <$> uncurry dists <*> snd $ splitVec xs
    splitVec vec   = (V.head vec,V.tail vec)

dists
 :: U.Vector Double
 -> V.Vector (U.Vector Double)
 -> U.Vector Double
dists pt = U.fromList . V.toList . V.map (distFunc pt)

distFunc ::  U.Vector Double -> U.Vector Double -> Double
distFunc xs ys = U.sum . U.zipWith diffsq xs $ ys
  where
    diffsq x y = (x-y) * (x-y)


combineHalves :: Int -> V.Vector (U.Vector Double) -> Maybe (U.Vector Double)
combineHalves numK built = f' <$> getExistDLH numK <*> getExistDRH numK $ built
  where
    f' ls rs = case rs of
               Nothing -> case ls of
                            Nothing -> Nothing
                            Just z  -> Just . U.snoc
                                                z $ (1/0 :: Double)
               Just x  -> case ls of
                            Nothing ->  Just $ U.cons
                                                  (1/0 :: Double) x
                            Just y  ->  Just $ (U.++) (U.snoc
                                                         y (1/0 :: Double) ) x

getExistDLH :: Int -> V.Vector (U.Vector Double) -> Maybe (U.Vector Double)
getExistDLH num built = clean . func $ Prelude.map getBuilt $ getDiag num
  where
    getDiag n          =  [(i,j) | i <- [0..n-1], j <- [1..n], i + j == n]
    getBuilt (row,col) = fmap (U.!? col) ((V.!? row) built)
    func               = dropWhile
                           (\x -> isNothing x  || (fromJust
                                                     . fmap isNothing $ x ))
    clean              = clean2 . clean1
    clean2             = fmap
                             U.fromList  . (\y -> if null y
                                                  then Nothing
                                                  else Just y)
    clean1             = Prelude.map (fromJust . fromJust)


getExistDRH :: Int -> V.Vector (U.Vector Double) -> Maybe (U.Vector Double)
getExistDRH num = (V.!? num)
