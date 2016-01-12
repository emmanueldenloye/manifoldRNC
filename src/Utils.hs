module Utils where

import           Control.Monad         (ap, liftM, liftM2)
import           Data.Char             (isNumber)
import           Data.List             (genericLength)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import           Numeric.LinearAlgebra (Matrix, cmap, cols, dropColumns,
                                        flatten, fromRows, loadMatrix,
                                        takeColumns, toList, toRows)
import qualified Numeric.LinearAlgebra as L
import           System.FilePath       (splitFileName)
import           System.Posix          (fileExist)

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
fileArgs = (=<<) fileExist . return . head

intCheck :: Foldable t => t Char -> Bool
intCheck x =  (not . null $ x) && foldr ((&&) . isNumber) True x

gtZero :: [String] -> Bool
gtZero = all ((> 0) . (\x-> read x :: Int) )

procMNIST :: [Double] -> Matrix Double -> Matrix Double
procMNIST selection mat = images
  where
    images = fromRows [x | (x,c) <- zip xs cs, or $ sequenceA eqfns c]
    xs     = toRows . cmap
                          (\x -> 255 - x :: Double)  . mSel takeColumns $ mat
    cs     = toList $ flatten . mSel dropColumns $ mat
    eqfns  = [(==x) | x <- selection]
    mSel f x = f  ((+ (-1)) $ cols x) x

checkArgs :: [String] -> (Int,Int)
checkArgs args = if ((== (3 :: Integer)) . genericLength) args
                 then let val = tail args
                      in if all intCheck val && gtZero val
                         then head . ap zip tail . Prelude.map read $ val -- yeah...
                         else error (errorMsg1 ++ usage)
                 else error (errorMsg2 ++ usage)

checkFile :: [FilePath] -> IO FilePath
checkFile args = if not . null $ args
                  then liftM2 getFile <$> return . head <*> fileArgs $ args
                  else error usage

getReqMatrix :: FilePath -> IO (Matrix Double)
getReqMatrix file = if (snd . splitFileName $ file) == "mnist.txt"
               then liftM (procMNIST [4]) $ loadMatrix file
               else loadMatrix file

convertDataset :: Matrix Double -> V.Vector (U.Vector Double)
convertDataset = V.map (U.fromList . L.toList) . V.fromList . L.toRows

rowSize :: Matrix Double -> Int
rowSize = L.rows
