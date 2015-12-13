module InterpolationAlgorithms
  (getQuadCoeffs,projmat,getCov)
  where

import Numeric.LinearAlgebra

-- getQuadCoeffs
--   :: Matrix Double
--   -> Matrix Double
--   -> Matrix Double
-- getQuadCoeffs ptdists projs = res
--   where
--     buildRect = L.fromRows . Prelude.map rowBuilder $ projs
--     rowBuilder rw = L.vjoin[1,L.vector rw,L.flatten $ L.vector rw `L.outer` L.vector rw]
--     res = (L.<\>) buildRect ptdists

getQuadCoeffs
  :: Matrix Double
  -> [[Double]]
  -> Matrix Double
getQuadCoeffs ptdists projs = res
  where
    buildRect = fromRows . Prelude.map rowBuilder $ projs
    rowBuilder rw = vjoin[1,vector rw,flatten $ vector rw `outer` vector rw]
    res = (<\>) buildRect ptdists

-- projmat :: Matrix Double -> Matrix Double
-- projmat points' = let themat = getCov 2 points'
--                    in runST $ do mo <- thawMatrix points'
--                                  mc <- thawMatrix themat

                    -- map (themat #>) $ toRows points'

projmat :: Matrix Double -> Matrix Double -- [Vector Double]
projmat points' = snd $ meanCov points'

  -- getCov 2 points'
                   -- in map (themat #>) $ toRows points'


getCov :: Int -> Matrix Double -> Matrix Double
getCov num basis = filtEVMat . innerEigenfunc $ basis
   where
    innerEigenfunc = snd . eigSH . trustSym . snd . meanCov
    filtEVMat      = tr . takeColumns num


-- projmat :: V.Vector (U.Vector Double) -> [[Double]]
-- projmat points' = let themat = getCov 2 (L.fromRows . V.toList $ vals')
--                       vals' = V.map (L.fromList . U.toList) points'
--                   in ($ vals') (V.toList . V.map (L.toList . (L.#>) themat))


-- getDerivatives'
--   :: Matrix Double
--   -> [Int]
--   -> Matrix Double
--   -> [(Double,Double)]
-- getDerivatives' points' inds' shortMat = coeffs
--   where coeffs = ap zip tail . join . L.toLists $
--                     getQuadCoeffs vv (projmat points')
--                     L.??
--                     (L.Range 1 1 2 ,L.All) /(-2)
--         filtRows = flip (L.??)  (L.Pos (L.idxs inds'), L.All)
--         vv = filtRows shortMat




-- getDerivatives'
--   :: V.Vector (U.Vector Double)
--   -> [Int]
--   -> Matrix Double
--   -> [(Double,Double)]
-- getDerivatives' points' inds' shortMat = coeffs
--   where coeffs = ap zip tail . join . L.toLists $
--                     getQuadCoeffs vv (projmat points')
--                     L.??
--                     (L.Range 1 1 2 ,L.All) /(-2)
--         filtRows = flip (L.??)  (L.Pos (L.idxs inds'), L.All)
--         vv = filtRows shortMat
