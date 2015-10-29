module NearestNeighbor
(selectK, combineHalves) where

import           Data.List (sort)
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Numeric.LinearAlgebra.HMatrix as H

selectK
  :: Int
  -> H.Vector Double
  -> V.Vector (H.Vector Double)
  -> Maybe (V.Vector (Int, H.Vector Double, Double))
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
    clean = fmap (uncurry $ flip (,)) . V.indexed . fromJust
    sortV = sort . V.toList

simpleBuild :: V.Vector (H.Vector Double) -> V.Vector (V.Vector Double)
simpleBuild = V.unfoldr (\x -> if V.null . V.tail $ x
                               then Nothing
                               else Just $ distVecPair x)
  where
    distVecPair xs = (,) <$> uncurry dists <*> snd $ splitVec xs
    splitVec vec   = (V.head vec,V.tail vec)

dists
    :: (Fractional b, S.Storable b)
    => S.Vector b
    -> V.Vector (S.Vector b)
    -> V.Vector b
dists pt = V.map (distFunc pt)
  where
    distFunc xs ys = S.sum . S.zipWith diffsq xs $ ys
    diffsq x y     = (x - y) * (x - y)

combineHalves :: Int -> V.Vector (V.Vector Double) -> Maybe (V.Vector Double)
combineHalves numK built = f' <$> getExistDLH numK <*> getExistDRH numK $ built
  where
    f' ls rs = case rs of
               Nothing -> case ls of
                            Nothing -> Nothing
                            Just z  -> Just . V.snoc
                                                z $ (1/0 :: Double)
               Just x  -> case ls of
                            Nothing ->  Just $ V.cons
                                                  (1/0 :: Double) x
                            Just y  ->  Just $ (V.++) (V.snoc
                                                         y (1/0 :: Double) ) x

getExistDLH :: Int -> V.Vector (V.Vector Double) -> Maybe (V.Vector Double)
getExistDLH num built = clean . func $ Prelude.map getBuilt $ getDiag num
  where
    getDiag n          =  [(i,j) | i <- [0..n-1], j <- [1..n], i + j == n]
    getBuilt (row,col) = fmap (V.!? col) ((V.!? row) built)
    func               = dropWhile
                           (\x -> isNothing x  || (fromJust
                                                     . fmap isNothing $ x ))
    clean              = clean2 . clean1
    clean2             = fmap
                             V.fromList  . (\y -> if null y
                                                  then Nothing
                                                  else Just y)
    clean1             = Prelude.map (fromJust . fromJust)


getExistDRH :: Int -> V.Vector (V.Vector Double) -> Maybe (V.Vector Double)
getExistDRH num = (V.!? num)
