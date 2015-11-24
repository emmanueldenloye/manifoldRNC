{-#LANGUAGE BangPatterns#-}
import           Control.Monad
import           Data.Char (isNumber)
import qualified Data.Graph.Inductive as G
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List hiding ((++),head,last)
import           Data.Maybe
import           Data.Vector as V hiding ((++),concatMap,zip,or,foldr,all,null,head,last)
import qualified Data.Vector.Unboxed as U hiding (or,foldr,all,null)
import           GraphAlgorithms
import           GraphBuilder
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           InterpolationAlgorithms
import           NearestNeighbor
import           Numeric.LinearAlgebra as L
import           Numeric.LinearAlgebra.HMatrix as H
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.Files
import           System.Random
import           Data.Array.Repa
import           PCA

type Weight = Double
type Graph r = Array r DIM2 Weight

usage :: String
usage = "Usage: ./init_present <file> <Graph nbd size> <PCA nbd size>"

errorMsg1 :: String
errorMsg1 = "You did not enter two positive integers.\n"

errorMsg2 :: String
errorMsg2 = "You did not enter two positive integer arguments.\n"

graphMsg1 :: String
graphMsg1 = "The graph is not fully connected. Please enter larger integers!"

fileMsg :: String
fileMsg = "You have not provided an existent file. Please try again."

getFile :: FilePath -> Bool -> FilePath
getFile file pCond = if pCond
                       then file
                       else error fileMsg

fileArgs :: [String] -> IO Bool
fileArgs = (=<<) fileExist . return . Prelude.head

intCheck :: Foldable t => t Char -> Bool
intCheck x = (not . null $ x) && foldr ((&&) . isNumber)  True x

gtZero :: [String] -> Bool
gtZero = all ((> 0) . (\x-> read x :: Int) )

procMNIST :: [Double] -> H.Matrix Double -> H.Matrix Double
procMNIST selection mat = images
  where
    images = fromRows [x | (x,c) <- zip xs cs, or $ sequenceA eqfns c]
    xs     = toRows . cmap
                          (\x -> 255 - x :: Double)  . mSel takeColumns $ mat
    cs     = H.toList $ flatten . mSel dropColumns $ mat
    eqfns  = [(==x) | x <- selection]
    mSel f x = f  ((+ (-1)) $ cols x) x

pp2DResults' :: FilePath -> [(Double,Double)] -> IO ()
pp2DResults' file xs = writeFile file . transPairLine $ xs
  where
    transPairLine = Prelude.concatMap
                (\x -> show (fst x) Prelude.++ " " Prelude.++ show (snd x) Prelude.++ "\n")


main :: IO ()
main =
  do args <- getArgs
     file <- if not . Prelude.null $ args
              then liftM2 getFile <$> return . Prelude.head <*> fileArgs $ args
              else error usage
     rawImages <- if (snd . splitFileName $ file) == "mnist.txt"
                   then liftM (procMNIST [4]) $ L.loadMatrix file
                   else L.loadMatrix file
     let nbds =  if ((== (3 :: Integer)) . genericLength) args
                 then let val = Prelude.tail args
                       in if Prelude.all intCheck val && gtZero val
                           then val
                           else error (errorMsg1 Prelude.++ usage)
                  else error (errorMsg2 Prelude.++ usage)
     let (len,images) = (,) <$> fst . H.size <*> V.map (U.fromList . L.toList)
                        . V.fromList . H.toRows $ rawImages
     let nodeEnum = U.enumFromN (0 :: Int) len
     -- let nearestTest = selectKtest images len
     -- print . selectKRepa images len (read $ Prelude.head nbds :: Int) $ 0
     -- basePoint <- randomRIO (0,V.length images - 1)
     let basePoint = 0
     let repafunc' = selectKRepa images len (read $ Prelude.head nbds :: Int)
     let (points',inds') = (\(_,a,b) -> (a,Data.List.sort $ U.toList b)) $ repafunc' basePoint
     -- let len' = U.length inds'
     let repaInds' = join $ Prelude.map (\x -> [x*len..(len*(x+1)-1)]) $ Prelude.take 3 inds'
     let shortMat = let repa' = fromUnboxed (Z:.len:.len) .
                                U.concatMap
                                ((\(a,_,_) -> a) . repafunc')
                                $ nodeEnum :: Graph U
                        !sp' = shortestPaths repa'
                    in selectP (or <$> sequenceA [(==) x | x <- repaInds']) (linearIndex sp') (len*len) :: IO (Array U DIM1 Double)
     val <- shortMat
     print val
     let projections = let themat = getCov 2 (L.fromRows . V.toList $ vals')
                           vals' = V.map (L.fromList . U.toList) points'
                       in foldr ((:) . L.toList . (L.#>) themat) [] vals'
     print "I fucked up! Fix this tommorow morning!"
     -- print inds'
     -- print $ Prelude.take (2*len) repaInds'

     -- print shortMat
     -- let ptdists = let ptdists' =  L.matrix (U.length points') . Data.Array.Repa.toList $ Data.Array.Repa.slice (shortestPaths shortMat) (Any :. (basePoint::Int))
     --                   in
     -- print . extent . computeP . Data.Array.Repa.traverse shortMat (\(e :. _) -> e) (\f (Z :.))
     -- print projections
     -- let ret' = Data.Array.Repa.slice shortMat (Any :. (0::Int) :. Data.Array.Repa.All)
     -- print $ (Data.Array.Repa.toList $ (computeS ret' :: Array U DIM1 Double)) == (U.toList . (\(a,_,_) -> a) . selectKRepa images len (read $ Prelude.head nbds :: Int) $ 0)

     -- print test

     -- let res' = gradInterpolation graph images 2 (nearest (read $ (!! 1) nbds :: Int)) basePoint
     -- print $ Data.Array.Repa.toList test2

     -- let nearest = selectK images lenkeys
     -- let graph = buildGraph (nearest (read $ Prelude.head nbds :: Int)) nodeEnum
     -- print . seq graph $ G.isConnected graph

     -- basePoint <- randomRIO (1,V.length images)
     -- print . IntMap.toList . snd . nearestTest (read $ Prelude.head nbds :: Int) $ 0
     -- let basePoint = 1 :: Int
     -- let graphMap = Data.List.foldr
     --                (\val map' -> IntMap.insert val
     --                              (nearestTest (read $ Prelude.head nbds :: Int)
     --                               val) map')
     --                IntMap.empty
     --                nodeEnum
     -- print . Prelude.take 2 . IntMap.toList $ graphMap
     -- print $ seq (shortestPaths nodeEnum graphMap) "What"
     -- print graphMap
     -- print $ Prelude.take 10 $ IntMap.elems graphMap
     -- print $ IntMap.size graphMap
     -- let nns = fromJust $ IntMap.keys <$> IntMap.lookup 200 graphMap
     -- print nns
     -- print $ IntMap.size <$> IntMap.lookup 0 graphMap
     -- let v1 = Prelude.map fst . (\(a,_,_,_) -> a) . selectK images len (read $ Prelude.head nbds :: Int) $ basePoint

     -- let v2 = snd . nearestTest (read $ Prelude.head nbds :: Int) $ basePoint
     -- print . shortestPaths (IntMap.keys . nearestTest (read $ Prelude.head nbds :: Int) $ basePoint) $ graphMap
     -- print v1
     -- print $ IntMap.elems v2
     -- let test = IntMap.lookup 10 graphMap
     -- print $ test <*> pure 10
     -- let wat = fromMaybe IntMap.empty (IntMap.lookup 20 graphMap)
     -- print . IntMap.size $ graphMap

     -- print $ G.isConnected graph -- for testing
     -- if G.isConnected graph
     --   then let res' = gradInterpolation graph images 2 (nearest (read $ (!! 1) nbds :: Int)) basePoint
     --            in print res'
     -- else error "What!"

     -- if G.isConnected graph
     --    then let results = let res' = gradInterpolation graph images 2 (nearest (read $ (!! 1) nbds :: Int)) basePoint
     --                            in Prelude.map ((,) <$> head <*> last) res' -- two dimensional
     --             in let plotFileName = "normcoords-"
     --                                   ++  "-" ++ show (basePoint + 1)
     --                                   ++ ":" ++ show  (V.length images)
     --                                   ++ ".png"
     --                    dataFileName = "normcoords-"
     --                                   ++ show (basePoint + 1)
     --                                   ++ ":" ++  show (V.length images)
     --                                   ++ ".txt"
     --                in  toFile def plotFileName (do layout_title .= "Normal Coordinates"
     --                                                setColors [opaque red]
     --                                                plot (points "points" results))
     --                    >> pp2DResults' dataFileName results
     --   else putStrLn graphMsg1

-- <<shortestPaths
shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0                                                -- <2>
  where
    Z :. _ :. n = extent g0                                               -- <1>

    go !g !k | k == n    = g                                              -- <3>
             | otherwise =
                 let g' = computeS (fromFunction (Z:.n:.n) sp)            -- <4>
                 in  go g' (k+1)                                          -- <5>
     where
       sp (Z:.i:.j) = min (g Data.Array.Repa.! (Z:.i:.j)) (g Data.Array.Repa.! (Z:.i:.k) + g Data.Array.Repa.! (Z:.k:.j)) -- <6>
-- >>
-- {-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
