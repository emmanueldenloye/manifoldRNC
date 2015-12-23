module InterpolationAlgorithms
  (riemannNormCoord,estimateGrad)
  where

import           Control.Monad
import           Control.Monad.ST
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

consInterpMat :: Int
              -> Int
              -> [Vector Double]
              -> Matrix  Double
consInterpMat rowlen collen vecs = runST $ do
                                    m <- thawMatrix mat
                                    mapM_ (\(i,v) -> write' m i $ flatOuter v) vecs'
                                    freezeMatrix m
  where
    flatOuter vec' = vjoin[1,vec',flatten . join outer $ vec'] :: Vector Double
    collen' = 1 + collen + (collen * collen)
    mat = konst 0 (rowlen, collen') :: Matrix Double
    vecs' = zip [0 :: Int ..] vecs
    write' m i v' = setMatrix m i 0 (asRow v')

estimateGrad :: Int
             -> Int
             -> Matrix Double
             -> Matrix Double
estimateGrad nums gradlen pcaMat = mat
  where
   mat = consInterpMat nums gradlen pcaMat'
   pcaMat' = map (subVector 1 gradlen . (<# getPCAEigVec pcaMat)) matRows
   matRows = toRows pcaMat

riemannNormCoord :: Matrix Double -> Matrix Double -> [Vector Double]
riemannNormCoord res mat = toColumns leastsquaresSol
  where
    leastsquaresSol = cmap (* (-0.5)) $ (res <\> mat) ?? (Pos $ idxs [1,2],All)

getPCAEigVec :: Matrix Double -> Matrix Double
getPCAEigVec = snd . eigSH . trustSym . snd . meanCov
