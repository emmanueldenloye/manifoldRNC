module InterpolationAlgorithms
  (getQuadCoeffs,getCov,estimateGrad,saveRes)
  where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import Control.Monad
import Control.Monad.ST

consInterpMat :: Int -> Int -> [Vector Double] -> Matrix  Double
consInterpMat rowlen collen vecs = runST $ do
                                    m <- thawMatrix mat
                                    mapM_ (\(i,v) -> write' m i (flatOuter v) ) vecs'
                                    freezeMatrix m
  where
    flatOuter vec' = vjoin[1,vec',flatten . join outer $ vec'] :: Vector Double
    collen' = 1 + collen + (collen * collen)
    mat = konst 0 (rowlen,collen') :: Matrix Double
    vecs' = zip [0 :: Int ..] vecs
    write' m i v' = setMatrix m i 0 (asRow v')

estimateGrad :: Int
             -> Int
             -> Int
             -> Matrix Double
             -> Matrix Double
             -> [Vector Double]
estimateGrad dims nums gradlen pcaMat = map (getQuadCoeffs dims mat) . toColumns
 where
   mat = consInterpMat nums gradlen pcaMat'
   pcaMat' = map (<# getCov gradlen pcaMat) $ toRows pcaMat

saveRes :: (Int,Int) -> String -> Int -> [Vector Double] -> IO ()
saveRes (gn,pn) file' bpt vecs = saveMatrix fileName "%.2f" mat
  where
    mat = fromRows vecs
    fileName = "results/hopefullyFinal/results" ++ "-" ++ file' ++ "-" ++ "base" ++ show bpt ++ "-" ++  show gn ++ "-" ++ show pn ++ ".txt"

getQuadCoeffs :: Int
              -> Matrix Double
              -> Vector Double
              -> Vector Double
getQuadCoeffs dims mat sdists = fix $ mat <\> asColumn sdists
  where
    fix = cmap (* (-0.5)) . subVector 1 dims . flatten

getCov :: Int -> Matrix Double -> Matrix Double
getCov num basis = filtEVMat . innerEigenfunc $ basis
   where
    innerEigenfunc = snd . eigSH . trustSym . snd . meanCov
    filtEVMat      = takeColumns num
