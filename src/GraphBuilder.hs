{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE BangPatterns#-}
{-#LANGUAGE RankNTypes#-}
{-#LANGUAGE DeriveGeneric#-}
module GraphBuilder
       (shortDistMat
       ,shortestDists
       ,joinContext
       ,EContext(..)
       ,theContextsAndDists
       )
       where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (join)
import           Control.Monad.ST
import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP
import           Data.List (sortOn)
import qualified Data.Strict.Tuple as T hiding (uncurry)
import qualified Data.Vector.Storable as S (drop)
import           Foreign.C (CInt)
import           GHC.Generics
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

data EContext = EContext {vertex     :: {-#UNPACK#-} !Int
                         , dists     :: {-#UNPACK#-} !(Vector Double)
                         , neighbors :: {-#UNPACK#-} !(Vector Int)
                         } deriving (Show,Generic)

instance NFData EContext

instance NFData EShortDists

data EShortDists = EShortDists {vertex' :: {-#UNPACK#-} !Int
                               , dists' :: {-#UNPACK#-} !(Vector Double)
                               } deriving (Show,Generic)

shortDistMat :: Int
             -> [EShortDists]
             -> Matrix Double
shortDistMat size' !sdists = let mat = konst 0 (size',size') :: Matrix Double
                             in runST $ do m <- thawMatrix mat
                                           mapM_ (`setRowAndCol` m) sdists
                                           freezeMatrix m
  where
    setRowAndCol (EShortDists pt dvals) mat = do setMatrix mat pt pt
                                                   (asRow $ S.drop pt dvals)
                                                 setMatrix mat pt pt
                                                   (asColumn $ S.drop pt dvals)

joinContext :: EContext -> GP.Gr () Double -> GP.Gr () Double
joinContext (EContext pt edists vs) graph = let !zipped = zip (toList edists) (toList vs)
                                              in (zipped,pt,(),zipped) & graph

matD2 :: Matrix Double -> Matrix Double
matD2 = join pairwiseD2

theContextsAndDists :: (Int,Int)
                    -> Int
                    -> Matrix Double
                    -> ([EContext],Matrix Double)
theContextsAndDists (n1,n2) bpt mat = (makeContexts . theNearest n1 . matD2 $ mat
                                      ,  (\a -> mat ?? (Pos a,All)) . idxs . map (toEnum . snd) . (!! bpt) . theNearest n2 . matD2 $ mat)
  where
    makeContexts = zipWith makeContext [0 :: Int ..] . map unzip
    makeContext a (b,c) = EContext a (fromList b) (fromList c)

theNearest :: Int -> Matrix Double -> [[(Double,Int)]]
theNearest n = filterKNearest . findNearestWithInd
  where
    findNearestWithInd = helper1 . helper2
    filterKNearest = map (take n . filter ((/= 0.0) . fst) . uncurry zip)
    helper1 in' = uncurry zip (map toList (T.fst in'), map (map fromEnum . toList) (T.snd in'))
    helper2 = strUnzip . map (\x -> (T.:!:) (sortVector x) (sortIndex x)) . toRows

shortestDists :: Int -> GP.Gr () Double -> [EShortDists]
shortestDists len g = map (\x -> clean x . sortIndDumpDist . makeList $ x) list'
  where
    list' = [0 :: Int ..len-1]
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
