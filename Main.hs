import           DenseGraph
import           InterpolationAlgorithms
import           NearestNeighbor
import           Plots
import           System.Environment
import           System.Random
import           Utils

main :: IO ()
main = do
  (len,file,nbds,images) <- do
                     args' <- getArgs
                     file' <- checkFile args'
                     images' <- getReqMatrix file'
                     let nbds' = checkArgs args'
                       in return $ (,,,) <$> getLength
                          <*> const file'
                          <*> const nbds'
                          <*> convertImages $ images'

  ((mat,points',inds'),basePoint) <- do
                     let f' = selectK images len
                     basePoint' <- randomRIO (0,len :: Int) :: IO Int
                     return $ (,)
                       (buildShortestPaths f' nbds basePoint')
                       basePoint'

  res <- getDerivatives' points' inds' <$> getConnected mat
  plotAndSave' file len res basePoint
