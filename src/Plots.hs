{-#OPTIONS_GHC -w#-}
module Plots
       (plotAndSave2D')
       where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.FilePath (takeBaseName)

plotAndSave2D'
  :: FilePath
  -> Int
  -> [(Double, Double)]
  -> Int
  -> IO ()
plotAndSave2D' file len results basePoint =
  do putStrLn "Plotting..."
     toFile def plotFileName $ chartPlot2D results
     putStrLn $ "Writing data points to file: " ++ dataFileName
     pp2DResults' dataFileName results
  where
   actualFileName = takeBaseName file
   baseOutputName = "normcoords-" ++ actualFileName
              ++ "-" ++ show (basePoint + 1)
              ++ ":" ++ show len
   plotFileName = baseOutputName ++ ".png"
   dataFileName = baseOutputName ++ ".txt"

chartPlot2D results = do layout_title .= "Normal Coordinates"
                         setColors [opaque red]
                         plot (points "points" results)

pp2DResults' :: FilePath -> [(Double,Double)] -> IO ()
pp2DResults' file xs = writeFile file . transPairLine $ xs
  where
    transPairLine = concatMap
                (\x -> show (fst x) ++ " " ++ show (snd x) ++ "\n")
