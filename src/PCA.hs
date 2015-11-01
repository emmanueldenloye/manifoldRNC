module PCA
       ( getBases
       , getPcaNbds
       ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Numeric.LinearAlgebra.HMatrix as H
import qualified Numeric.LinearAlgebra as L
import NearestNeighbor (selectK)

getPcaNbds
    ::  Int
    ->  V.Vector (U.Vector Double)
    ->  V.Vector (Maybe (V.Vector (Int, U.Vector Double)))
getPcaNbds numK xs = V.map (getBasisVect numK xs) xs

getBasisVect
    :: Int
    -> V.Vector (U.Vector Double)
    -> U.Vector Double
    -> Maybe (V.Vector (Int, U.Vector Double))
getBasisVect num setx pt = case selectK num pt setx of
                          Just x -> Just . fmap
                                             (\(a,b,_) -> (a,b))  $ x
                          Nothing -> Nothing

getBases
  :: V.Vector (Maybe (V.Vector (Int, U.Vector Double)))
  -> V.Vector (Maybe (H.Matrix Double))
getBases = V.map
           (fmap $ getCov 2 . H.fromRows
            . V.toList . V.map (L.fromList . U.toList . snd))

getCov :: Int -> H.Matrix Double -> H.Matrix Double
getCov num basis = filtEVMat . innerEigenfunc $ basis
   where
    innerEigenfunc = snd . L.eigSH . H.trustSym . snd . L.meanCov
    filtEVMat      = H.tr . H.fromColumns . getEigVectOrdtakeN num

getEigVectOrdtakeN
    :: Int
    -> H.Matrix Double
    -> [H.Vector Double]
getEigVectOrdtakeN num cov = eigVect
  where
    eigSys  = H.eigSH (H.trustSym cov)
    eigVect = Prelude.take num . H.toColumns . snd $ eigSys
    -- eigVal  = S.take num . fst $ eigSys
