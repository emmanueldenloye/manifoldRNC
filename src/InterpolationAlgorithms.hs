module InterpolationAlgorithms
  (getQuadCoeffs,projmat,getDerivatives')
  where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Numeric.LinearAlgebra as L
import Numeric.LinearAlgebra (Matrix)
import           PCA
import Control.Monad (ap, join)

getQuadCoeffs
  :: Matrix Double
  -> [[Double]]
  -> Matrix Double
getQuadCoeffs ptdists projs = res
  where
    buildRect = L.fromRows . Prelude.map rowBuilder $ projs
    rowBuilder rw = L.vjoin[1,L.vector rw,L.flatten $ L.vector rw `L.outer` L.vector rw]
    res = (L.<\>) buildRect ptdists

projmat :: V.Vector (U.Vector Double) -> [[Double]]
projmat points' = let themat = getCov 2 (L.fromRows . V.toList $ vals')
                      vals' = V.map (L.fromList . U.toList) points'
                  in ($ vals') (V.toList . V.map (L.toList . (L.#>) themat))

getDerivatives'
  :: V.Vector (U.Vector Double)
  -> [Int]
  -> Matrix Double
  -> [(Double,Double)]
getDerivatives' points' inds' shortMat = coeffs
  where coeffs = ap zip tail . join . L.toLists $
                    getQuadCoeffs vv (projmat points')
                    L.??
                    (L.Range 1 1 2 ,L.All) /(-2)
        filtRows = flip (L.??)  (L.Pos (L.idxs inds'), L.All)
        vv = filtRows shortMat
