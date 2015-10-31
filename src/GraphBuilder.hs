module GraphBuilder
       (buildGraph
       ,capTrees
       ,shakeNodes
       ,shakePoints
       ) where

import Data.Graph.Inductive
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra.HMatrix as H
import Data.List
import Data.Maybe
import NearestNeighbor (selectK)

buildGraph
  :: DynGraph gr
  => Int
  -> V.Vector (H.Vector Double)
  -> gr () Double
buildGraph numK setx = buildGr . filtCxts . cleanzip $ gcontexts
  where
    gcontexts    = V.toList $ V.map (fromMaybe [] . succN) setx
    filtCxts    = Prelude.map
                    (\(a,b,c,d) -> (filtDups b a, b, c,  filtDups b d))
    filtDups xs = Prelude.filter (\x -> Prelude.snd x > xs)
    succN y     = getUnDirSucc numK y setx
    cleanzip x  = Data.List.zip4 x [0..V.length setx - 1] symbols x
    symbols     = Prelude.repeat ()

getUnDirSucc
  :: Int
  -> H.Vector Double
  -> V.Vector (H.Vector Double)
  -> Maybe (Adj Double)
getUnDirSucc numK pt setx = case selectK numK pt setx of
                             Just x -> Just . V.toList
                                         . fmap (\(a,_,b) -> (b,a)) $ x
                             Nothing -> Nothing

capTrees
  :: Graph gr
  =>  gr () Double
  -> Maybe (V.Vector Node)
  -> Maybe (V.Vector (V.Vector Double))
capTrees grp = fmap $ V.map (V.tail . V.fromList
                              . map snd . Data.List.sort
                              . map (head . unLPath)
                              . flip spTree grp)

shakeNodes
  :: V.Vector (Maybe (V.Vector (Int, H.Vector Double)))
  -> V.Vector (Maybe (V.Vector Int))
shakeNodes = V.map (fmap $ V.map fst)

shakePoints
  :: V.Vector (Maybe (V.Vector (Int, H.Vector Double)))
  -> V.Vector (Maybe (V.Vector (H.Vector Double)))
shakePoints = V.map (fmap $ V.map snd)
