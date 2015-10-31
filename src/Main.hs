import Control.Monad
import Data.Char (isNumber)
import Data.Graph.Inductive
import Data.List (genericLength)
import Data.Maybe
import Data.Vector as V hiding ((++),concatMap,zip,or,foldr,all,null)
import GraphBuilder
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import InterpolationAlgorithms
import Numeric.LinearAlgebra as L
import Numeric.LinearAlgebra.HMatrix as H
import PCA
import System.Environment
import System.FilePath.Posix
import System.Posix.Files
import System.Random

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

pp2DResults' :: Show a => FilePath -> [a] -> IO ()
pp2DResults' file xs = writeFile file . transPairLine . getPairs $ xs
  where
    transPairLine = concatMap
                (\x -> show (fst x) ++ " " ++ show (snd x) ++ "\n")

getPairs :: [t] -> [(t, t)]
getPairs []       = []
getPairs [x]      = [(x,x)]
getPairs [x,y]    = [(x,y)]
getPairs (x:y:xs) = (x,y):getPairs xs

runAnalysis
  :: V.Vector (H.Vector Double)
  -> Int
  -> Gr () Double
  -> Int
  -> Maybe (V.Vector (Double, Double))
runAnalysis imgs numK grp = gradInterpolation grp verts pts mats
 where
   nbds               = getPcaNbds numK imgs
   (verts, pts, mats) = (,,) <$> shakeNodes <*> shakePoints <*> getBases $ nbds

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
                           else error (errorMsg1 ++ usage)
                  else error (errorMsg2 ++ usage)
     let images = V.fromList . H.toRows $ rawImages
     let graph  = buildGraph
                     (read $ Prelude.head nbds :: Int) images
     basePoint <- randomRIO (0,V.length images - 1)
     if isConnected graph
       then let vals = V.toList <$>
                       runAnalysis images
                                  (read $ (!! 1) nbds :: Int)
                                  graph
                                  basePoint
                in if isJust vals
                    then let plotFileName = "normcoords-"
                                            ++  "-" ++ show (basePoint + 1)
                                            ++ ":" ++ show  (V.length images)
                                            ++ ".png"
                             dataFileName = "normcoords-"
                                            ++ show (basePoint + 1)
                                            ++ ":" ++  show (V.length images)
                                            ++ ".txt"
                     in  toFile def plotFileName (do
                         layout_title .= "Normal Coordinates"
                         setColors [opaque red]
                         plot (points "points" $ fromJust vals))
                         >> pp2DResults' dataFileName (fromJust vals)
                    else putStrLn "There is nothing to plot."
       else putStrLn graphMsg1