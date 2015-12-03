{-#OPTIONS_GHC -w#-}
module Plots
       (plotAndSave') where

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotAndSave'
  :: FilePath
  -> Int
  -> [(Double, Double)]
  -> Int
  -> IO ()
plotAndSave' file len results basePoint =
  do putStrLn "Plotting..."
     toFile def plotFileName $ chartPlot results
     putStrLn $ "Writing data points to file: " ++ dataFileName
     pp2DResults' dataFileName results
  where
   actualFile = reverse . takeWhile (/= '/') . reverse $ file
   baseName = "normcoords-" ++ show actualFile
              ++ "-" ++ show (basePoint + 1)
              ++ ":" ++ show len
   plotFileName = baseName ++ ".png"
   dataFileName = baseName ++ ".txt"

chartPlot results = do
             layout_title .= "Normal Coordinates"
             setColors [opaque red]
             plot (points "points" results)

pp2DResults' :: FilePath -> [(Double,Double)] -> IO ()
pp2DResults' file xs = writeFile file . transPairLine $ xs
  where
    transPairLine = concatMap
                (\x -> show (fst x) ++ " " ++ show (snd x) ++ "\n")
