module PCA
       ( getCov
       ) where

import qualified Numeric.LinearAlgebra.HMatrix as H
import qualified Numeric.LinearAlgebra as L

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
