module InterpolationAlgorithms
(gradInterpolation)
  where

import           Control.Monad
import           Data.Graph.Inductive
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import           GraphBuilder
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.HMatrix as H

getQuadCoeffs
  :: H.Field t
  => V.Vector (V.Vector t)
  -> V.Vector (H.Vector t)
  -> V.Vector (H.Vector t)
getQuadCoeffs interps coeffs = V.fromList . H.toColumns
                               . L.linearSolveLS buildRect $ depv
  where
    depv           = H.fromRows . V.toList
                     . V.map (S.fromList . V.toList) $ interps
    buildRect      = H.fromRows . V.toList . V.map rowbuilder $ coeffs
    rowbuilder cur = (S.++)
                     ((S.++) (S.fromList [1]) cur)
                     (H.flatten $ H.outer cur cur)

gradInterpolation
  :: Gr () Double
     -> V.Vector (Maybe (V.Vector Node))
     -> V.Vector (Maybe (V.Vector (H.Vector Double)))
     -> V.Vector (Maybe (H.Matrix Double))
     -> Int
     -> Maybe (V.Vector (Double, Double))
gradInterpolation grp verts pts mats num = convertToTuple . nodeGrad $ num
  where
    vars numNode       = liftM2 (\x -> V.map (H.<# H.tr x))
                        ((V.!) mats numNode)
                        ((V.!) pts numNode)
    depvars numNode    = capTrees
                        (grp :: Gr () Double)
                        ((V.! numNode) verts)
    quadCoeffs numNode = liftM2 getQuadCoeffs (depvars numNode) (vars numNode)
    nodeGrad numNode   = if isJust . vars $ numNode
                          then V.map
                          (H.cmap (* (-0.5)) . S.take
                           (H.size . V.head . fromJust . vars $ numNode)
                           . S.tail)
                          <$> quadCoeffs numNode
                          else Nothing
    convertToTuple     = liftM (V.map
                               (\x ->
                                  (S.head x, S.head . S.tail $
                                             (x :: H.Vector Double))))
   -- convertToTuple assumes the gradient is two dimensional, for now!!!
