{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module GraphBuilder
       (shortDistMat
       ,shortestDists
       ,joinContext
       ,EContext(..)
       ,EShortDists(..)
       ,theContextsAndDists
       )
       where

import           Control.DeepSeq                   (NFData (..))
import           Control.Monad                     (join)
import           Control.Monad.ST
import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP
import           Data.List                         (sortOn)
import qualified Data.Strict.Tuple                 as T hiding (uncurry)
import           Foreign.C                         (CInt)
import           GHC.Generics
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

data EContext = EContext {vertex     :: {-#UNPACK#-} !Int
                         , dists     :: {-#UNPACK#-} !(Vector Double)
                         , neighbors :: {-#UNPACK#-} !(Vector Int)
                         } deriving (Show,Generic)

instance NFData EContext

data EShortDists = EShortDists {vertex' :: {-#UNPACK#-} !Int
                               , dists' :: {-#UNPACK#-} !(Vector Double)
                               } deriving (Show,Generic)

instance NFData EShortDists

shortDistMat :: Int -> [EShortDists] -> Matrix Double
shortDistMat size' !sdists = let mat = konst 0 (length',size') :: Matrix Double
                                 length' = length sdists
                             in runST $ do m <- thawMatrix mat
                                           mapM_ (`setRow` m) $ zip sdists [0..]
                                           freezeMatrix m
  where
    setRow (e,pt) mat = setMatrix mat pt 0 . asRow . dists' $ e

joinContext :: EContext -> GP.Gr () Double -> GP.Gr () Double
joinContext (EContext pt edists vs) graph = let !zipped = zip (toList edists) (toList vs)
                                            in (zipped,pt,(),zipped) & graph

matD2 :: Matrix Double -> Matrix Double
matD2 = join pairwiseD2


theContextsAndDists :: (Int,Int) -- ^ Graph neighborhood size and PCA neighborhood size
                    -> Int
                    -> Matrix Double
                    -> ([EContext], Matrix Double,[Int])
theContextsAndDists (n1,n2) bpt mat = (makeContexts . theNearest n1 . matD2 $ mat
                                      , (\a -> mat ?? (Pos a,All)) . idxs $ idxs'',idxs''')
  where
    makeContexts = zipWith makeContext [0 :: Int ..] . map unzip
    makeContext a (b,c) = EContext a (fromList b) (fromList c)
    idxs'' = map (toEnum . snd) idxs'
    idxs' = (!! bpt) . theNearest n2 . matD2 $ mat
    idxs''' = map snd idxs'

theNearest :: Int -> Matrix Double -> [[(Double,Int)]]
theNearest n = filterKNearest . findNearestWithInd
  where
    findNearestWithInd = helper1 . helper2
    filterKNearest = map (take n . filter ((/= 0.0) . fst) . uncurry zip)
    helper1 in' = uncurry zip (map toList (T.fst in'), map (map fromEnum . toList) (T.snd in'))
    helper2 = strUnzip . map (\x -> (T.:!:) (sortVector x) (sortIndex x)) . toRows

shortestDists :: [Int] -> GP.Gr () Double -> [EShortDists]
shortestDists inds g = map (\x -> clean x . sortIndDumpDist . makeList $ x) inds
  where
    clean x' a = EShortDists x' $ fromList a
    sortIndDumpDist = map T.snd . sortOn T.fst
    makeList = map (Prelude.uncurry (T.:!:)) . join . map (take 1 . unLPath) . flip spTree g

-- UTILITY FUNCTION(S) --

strUnzip :: [T.Pair (Vector Double) (Vector CInt)] -> T.Pair [Vector Double] [Vector CInt]
strUnzip = foldr (\((T.:!:) a b) acc -> ((T.:!:) (a:T.fst acc) (b:T.snd acc))) ((T.:!:) [] [])

-- -- For debugging!
-- remember to import Control.Arrow ((&&&)) when using this function.
-- checkSymRowsCols :: (Container Vector a, Eq a, S.Storable a)
--                     => Matrix a
--                     -> Bool
-- checkSymRowsCols = uncurry (==) . (map sumElements . toColumns &&& map sumElements . toRows)
