{-#OPTIONS_GHC -w#-}
module Plots
       (plotAndSave2D')
       where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.FilePath (takeBaseName)
import Numeric.LinearAlgebra as L (toList,Vector(..),size)

plotAndSave2D'
  :: FilePath
  -> Int
  -> Int
  -> (Int,Int)
  -> [L.Vector Double]
  -> IO ()
plotAndSave2D' file len basePoint (gn,pn) results =
  if (L.size . head $ results) /= 2
        then error "This function only works for 2D points!"
         else do putStrLn "Plotting..."
                 toFile def plotFileName $ chartPlot2D results'
                 putStrLn $ "Writing data points to file: " ++ dataFileName
                 pp2DResults' dataFileName results'
  where
   actualFileName = takeBaseName file
   baseOutputName = actualFileName ++ "-"
                    ++ "base" ++ show basePoint ++
                    "-" ++  show gn ++ "-" ++ show pn
   plotFileName = baseOutputName ++ ".png"
   dataFileName = baseOutputName ++ ".txt"
   results' = map ((\[a,b] -> (a,b)) . L.toList) results


chartPlot2D results = do layout_title .= "Normal Coordinates"
                         setColors [opaque red]
                         plot (points "points" results)

pp2DResults' :: FilePath -> [(Double,Double)] -> IO ()
pp2DResults' file xs = writeFile file . transPairLine $ xs
  where
    transPairLine = concatMap
                (\x -> show (fst x) ++ " " ++ show (snd x) ++ "\n")
