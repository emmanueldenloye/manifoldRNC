-- I can provide the text files used for testing via Dropbox if necessary.
-- Thank you in advance to those who decide to read this. This is my first
-- "actual" Haskell project.
import           Control.Monad
import           Data.Char (isNumber)
import           Data.Graph.Inductive as GI
import           Data.List (sort,zip4,genericLength)
import           Data.Maybe
-- import           Data.Time.Format
-- import           Data.Time.LocalTime
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.HMatrix as H
import           System.Environment
import           System.FilePath.Posix
import           System.Posix.Files
import           System.Random

type Matx    = H.Matrix Double
type Ptx     = H.Vector Double
type Distances = V.Vector Double

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
getFile file cond = if cond
                       then file
                       else error fileMsg

fileArgs :: [String] -> IO Bool
fileArgs = (=<<) fileExist . return . Prelude.head

intCheck :: Foldable t => t Char -> Bool
intCheck x = (not . null $ x) && foldr ((&&) . isNumber)  True x

gtZero :: [String] -> Bool
gtZero = all ((> 0) . (\x-> read x :: Int) )

procMNIST :: [Double] -> Matx -> Matx
procMNIST selection mat = images
  where
    images = H.fromRows [x | (x,c) <- zip xs cs, or $ sequenceA eqfns c]
    xs     = H.toRows . H.cmap
                          (\x -> 255 - x :: Double)  . mSel H.takeColumns $ mat
    cs     = H.toList $ H.flatten . mSel H.dropColumns $ mat
    eqfns  = [(==x) | x <- selection]
    mSel f x = f  ((+ (-1)) $ H.cols x) x

pp2DResults' :: Show a => FilePath -> [a] -> IO ()
pp2DResults' file xs= writeFile file . transPairLine . getPairs $ xs
  where
    transPairLine = concatMap
                (\x -> show (fst x) ++ " " ++ show (snd x) ++ "\n")

getPairs :: [t] -> [(t, t)]
getPairs []       = []
getPairs [x]      = [(x,x)]
getPairs [x,y]    = [(x,y)]
getPairs (x:y:xs) = (x,y):getPairs xs

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
                       in if all intCheck val && gtZero val
                           then val
                           else error (errorMsg1 ++ usage)
                  else error (errorMsg2 ++ usage)
     let images = V.fromList . H.toRows $ rawImages
     let graph  = buildGraph
                     (read $ head nbds :: Int) images  :: Gr () Double
     basePoint <- randomRIO (0,V.length images - 1)
     -- curTime   <- map (\x -> if x == ' ' then '-' else x) . drop 4
     --              . formatTime defaultTimeLocale "%-z%c" <$> getZonedTime
     if GI.isConnected graph
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

runAnalysis
  :: V.Vector Ptx
  -> Int
  -> Gr () Double
  -> Int
  -> Maybe (V.Vector (Double, Double))
runAnalysis imgs numK grp = convertToTuple . nodeGrad
 where
   nbds               = getPcaNbds numK imgs
   (verts, pts, mats) = (,,) <$> shakeNodes <*> shakePoints <*> getBases $ nbds
   vars numNode       = liftM2 (\x -> V.map (H.<# H.tr x))
                        ((V.!) mats numNode)
                        ((V.!) pts numNode)
   depvars numNode    = capTrees
                        (grp :: Gr () Double)
                        ((V.! numNode) verts)
   quadCoeffs numNode = liftM2 getQuadCoeffs (depvars numNode) (vars numNode)
   nodeGrad numNode   = if isJust . vars $ numNode
                          then V.map
                          (H.cmap (* (-0.5)) . S.take
                           (H.size . V.head . fromJust . vars $ numNode)
                           . S.tail)
                          <$> quadCoeffs numNode
                          else Nothing
   convertToTuple     = liftM (V.map
                               (\x ->
                                  (S.head x, S.head . S.tail $
                                             (x :: H.Vector Double))))
   -- Converttotuple assumes the gradient is two dimensional, for now!!!

getQuadCoeffs
  :: H.Field t
  => V.Vector (V.Vector t)
  -> V.Vector (H.Vector t)
  -> V.Vector (H.Vector t)
getQuadCoeffs interps coeffs = V.fromList . H.toColumns
                               . L.linearSolveLS buildRect $ depv
  where
    depv           = H.fromRows . V.toList
                     . V.map (S.fromList . V.toList) $ interps
    buildRect      = H.fromRows . V.toList . V.map rowbuilder $ coeffs
    rowbuilder cur = (S.++)
                     ((S.++) (S.fromList [1]) cur)
                     (H.flatten $ H.outer cur cur)

shakeNodes
  :: V.Vector (Maybe (V.Vector (Int, Ptx)))
  -> V.Vector (Maybe (V.Vector Int))
shakeNodes = V.map (fmap $ V.map fst)

shakePoints
  :: V.Vector (Maybe (V.Vector (Int, Ptx)))
  -> V.Vector (Maybe (V.Vector Ptx))
shakePoints = V.map (fmap $ V.map snd)

getBases
  :: V.Vector (Maybe (V.Vector (a, Ptx)))
  -> V.Vector (Maybe (H.Matrix Double))
getBases = V.map
           (fmap $ getCov 2 . H.fromRows . V.toList . fmap snd)

getEigVectOrdtakeN
    :: Int
    -> Matx
    -> [H.Vector Double]
getEigVectOrdtakeN num cov = eigVect
  where
    eigSys  = H.eigSH (H.trustSym cov)
    eigVect = Prelude.take num . H.toColumns . snd $ eigSys
    -- eigVal  = S.take num . fst $ eigSys

getCov :: Int -> H.Matrix Double -> H.Matrix Double
getCov num basis = filtEVMat . innerEigenfunc $ basis
   where
    innerEigenfunc = snd . L.eigSH . H.trustSym . snd . L.meanCov
    filtEVMat      = H.tr . H.fromColumns . getEigVectOrdtakeN num


getPcaNbds
    :: Int
    -> V.Vector Ptx
    ->  V.Vector (Maybe (V.Vector (Int, Ptx)))
getPcaNbds numK xs = V.map (getBasisVect numK xs) xs

getBasisVect
    :: Int
    -> V.Vector Ptx
    -> Ptx
    -> Maybe (V.Vector (Int, Ptx))
getBasisVect num setx pt = case selectK num pt setx of
                          Just x -> Just . fmap
                                             (\(a,b,_) -> (a,b))  $ x
                          Nothing -> Nothing

capTrees
  :: Graph gr
  =>  gr () Double
  -> Maybe (V.Vector Node)
  -> Maybe (V.Vector (V.Vector Double))
capTrees grp = fmap $ V.map (V.tail . V.fromList
                              . map snd . Data.List.sort
                              . map (head . GI.unLPath)
                              . flip GI.spTree grp)

combineHalves :: Int -> V.Vector Distances -> Maybe Distances
combineHalves numK built = f' <$> getExistDLH numK <*> getExistDRH numK $ built
  where
    f' ls rs = case rs of
               Nothing -> case ls of
                            Nothing -> Nothing
                            Just z  -> Just . V.snoc
                                                z $ (1/0 :: Double)
               Just x  -> case ls of
                            Nothing ->  Just $ V.cons
                                                  (1/0 :: Double) x
                            Just y  ->  Just $ (V.++) (V.snoc
                                                         y (1/0 :: Double) ) x

getExistDLH :: Int -> V.Vector Distances -> Maybe Distances
getExistDLH num built = clean . func $ Prelude.map getBuilt $ getDiag num
  where
    getDiag n          =  [(i,j) | i <- [0..n-1], j <- [1..n], i + j == n]
    getBuilt (row,col) = fmap (V.!? col) ((V.!? row) built)
    func               = dropWhile
                           (\x -> isNothing x  || (fromJust
                                                     . fmap isNothing $ x ))
    clean              = clean2 . clean1
    clean2             = fmap
                             V.fromList  . (\y -> if null y
                                                  then Nothing
                                                  else Just y)
    clean1             = Prelude.map (fromJust . fromJust)


getExistDRH :: Int -> V.Vector Distances -> Maybe Distances
getExistDRH num = (V.!? num)

simpleBuild :: V.Vector Ptx -> V.Vector Distances
simpleBuild = V.unfoldr (\x -> if V.null . V.tail $ x
                               then Nothing
                               else Just $ distVecPair x)
  where
    distVecPair xs = (,) <$> uncurry dists <*> snd $ splitVec xs
    splitVec vec   = (V.head vec,V.tail vec)

getUnDirSucc
  :: Int
  -> Ptx
  -> V.Vector Ptx
  -> Maybe (GI.Adj Double)
getUnDirSucc numK pt setx = case selectK numK pt setx of
                             Just x -> Just . V.toList
                                         . fmap (\(a,_,b) -> (b,a)) $ x
                             Nothing -> Nothing

buildGraph
  :: DynGraph gr
  => Int
  -> V.Vector Ptx
  -> gr () Double
buildGraph numK setx = GI.buildGr . filtCxts . cleanzip $ gcontexts
  where
    gcontexts    = V.toList $ V.map (fromMaybe [] . succN) setx
    filtCxts    = Prelude.map
                    (\(a,b,c,d) -> (filtDups b a, b, c,  filtDups b d))
    filtDups xs = Prelude.filter (\x -> Prelude.snd x > xs)
    succN y     = getUnDirSucc numK y setx
    cleanzip x  = Data.List.zip4 x [0..V.length setx - 1] symbols x
    symbols     = Prelude.repeat ()

selectK
  :: Int
  -> Ptx
  -> V.Vector Ptx
  -> Maybe (V.Vector (Int, Ptx, Double))
selectK numK pt setx = if isJust ptIdx && numK >=1
                         then fmap (V.fromList . mapP) fsort
                         else Nothing
  where
    fsort = if isJust comb
               then Just . Prelude.take numK . sortV . clean $ comb
               else Nothing
    ptIdx = V.elemIndex pt setx
    comb  = combineHalves (fromJust ptIdx) $ simpleBuild setx
    mapP  = map (\(a,b) -> (b,(V.!) setx b,a))
    clean = fmap (uncurry $ flip (,)) . V.indexed . fromJust
    sortV = Data.List.sort . V.toList

dists
    :: (Fractional b, S.Storable b)
    => S.Vector b
    -> V.Vector (S.Vector b)
    -> V.Vector b
dists pt = V.map (distFunc pt)
  where
    distFunc xs ys = S.sum . S.zipWith diffsq xs $ ys
    diffsq x y     = (x - y) * (x - y)
