module GraphBuilder
       (buildGraph
       ) where

import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP

buildGraph
  :: (Int -> Context () Double)
  -> [Int]
  -> GP.Gr () Double
buildGraph nearest = foldr ((&) . nearest) empty

-- capTrees :: GP.Gr () Double -> Int -> [Int] -> [Double]
-- capTrees graph pt = Prelude.map (\x -> flip3 G.spLength graph x pt)
--   where -- The last argument is nearest
--    flip3 f z y x = f x y z
