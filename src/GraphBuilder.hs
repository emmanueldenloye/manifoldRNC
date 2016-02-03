{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module GraphBuilder (
    shortDistMat,
    shortestDists,
    joinContext,
    EContext(..),
    EShortDists(..),
    theContextsAndDists,
    ) where

import           Control.DeepSeq                   (NFData (..))
import           Control.Monad                     (ap, join, liftM2)
import           Control.Monad.ST
import           Control.Parallel.Strategies
import           Data.Graph.Inductive              hiding (ap)
import qualified Data.Graph.Inductive.PatriciaTree as GP
import           Data.List                         (sortOn)
import qualified Data.Strict.Tuple                 as T hiding (uncurry)
import           Foreign.C                         (CInt)
import           GHC.Generics
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

data EContext =
       EContext
         { vertex   :: {-# UNPACK #-} !Int
         , dstToCtr :: {-# UNPACK #-} !Double
         , dists    :: {-# UNPACK #-} !(Vector Double)
         , ns       :: {-# UNPACK #-} !(Vector Int)
         }
  deriving (Show, Generic)

instance NFData EContext

data EShortDists =
       EShortDists
         { vertex' :: {-# UNPACK #-} !Int
         , dists'  :: {-# UNPACK #-} !(Vector Double)
         }
  deriving (Show, Generic)

instance NFData EShortDists

shortDistMat :: Int -> [EShortDists] -> Matrix Double
shortDistMat size' !sdists =
  let mat = konst 0 (length', size') :: Matrix Double
      length' = length sdists
  in runST $ do
    m <- unsafeThawMatrix mat
    mapM_ (`setRow` m) $ zip sdists [0 ..]
    unsafeFreezeMatrix m
  where
    setRow (e, pt) mat = setMatrix mat pt 0 . asRow . dists' $ e

joinContext :: EContext -> GP.Gr Double Double -> GP.Gr Double Double
joinContext (EContext pt dstCtr edists vs) graph =
  let zipped = zip (toList edists) (toList vs)
  in (zipped, pt, dstCtr, zipped) & graph

matD2 :: Matrix Double -> Matrix Double
matD2 = join pairwiseD2

theContextsAndDists :: (Int, Int) -- ^ Graph neighborhood size and PCA neighborhood size
                    -> Int
                    -> Matrix Double
                    -> ([EContext], Matrix Double, [Int])
theContextsAndDists (n1, n2) bpt mat =
  (makeContexts . theNearest n1 $ mat', extract' . idxs $ idxs'', idxs''')
  where
    makeContexts = zipWith makeContext [0 :: Int ..] . map unzip
    makeContext a (b, c) = EContext a (mat' `atIndex` (a, bpt)) (fromList b) (fromList c)
    mat' = matD2 mat
    idxs'' = map (toEnum . snd) idxs'
    idxs' = (!! bpt) . theNearest n2 $ mat'
    idxs''' = map snd idxs'
    extract' a = mat ?? (Pos a, All)

theNearest :: Int -> Matrix Double -> [[(Double, Int)]]
theNearest n = filterKNearest . findNearestWithInd
  where
    findNearestWithInd = helper1 . helper2
    filterKNearest = map (take n . filter ((/= 0.0) . fst) . uncurry zip)
    helper1 in' = uncurry zip (map toList (T.fst in'), map (map fromEnum . toList) (T.snd in'))
    helper2 = strUnzip . map (liftM2 (T.:!:) sortVector sortIndex) . toRows

shortestDists :: [Int] -> GP.Gr Double Double -> [EShortDists]
shortestDists inds g = parMap rseq (clean `ap` (sortIndDumpDist . makeList)) inds
  where
    clean x' a = EShortDists x' $ fromList a
    sortIndDumpDist = map T.snd . sortOn T.fst
    makeList = map (Prelude.uncurry (T.:!:)) . join . map (take 1 . unLPath) . flip spTree g

-- UTILITY FUNCTION(S) --

strUnzip :: [T.Pair (Vector Double) (Vector CInt)] -> T.Pair [Vector Double] [Vector CInt]
strUnzip = foldr (\((T.:!:) a b) acc -> ((T.:!:) (a : T.fst acc) (b : T.snd acc))) ((T.:!:) [] [])

-- -- For debugging!
-- remember to import Control.Arrow ((&&&)) when using this function.
-- checkSymRowsCols :: (Container Vector a, Eq a, S.Storable a)
--                     => Matrix a
--                     -> Bool
-- checkSymRowsCols = uncurry (==) . (map sumElements . toColumns &&& map sumElements . toRows)
