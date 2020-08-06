import           Data.List         (nub)
import qualified Data.Set as Set   (fromList, size, intersection)
import           Control.Monad     (mapM_)

data Dig = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pos = P1 | P2 | P3 | P4
  deriving (Eq, Show, Ord, Enum, Bounded)

data Combi = Combi Dig Dig Dig Dig deriving (Eq)
instance Show Combi where
  show (Combi d1 d2 d3 d4) = concat $ drop 1 . show <$> [d1, d2, d3, d4]

data Res = Res Int Int deriving (Eq)
instance Show Res where
  show (Res wholes halves) = show wholes ++ "● " ++ show halves ++ "◐"


evalWithHidden :: Combi -> Combi -> Res
evalWithHidden hidden@(Combi h1 h2 h3 h4) turn@(Combi t1 t2 t3 t4) = Res wholes halves
  where
    (hiddenList, turnList) = ([h1, h2, h3, h4], [t1, t2, t3, t4])   -- lists for zipping
    wholes = length . filter id $ zipWith (==) turnList hiddenList
    [hiddenSet, turnSet] = Set.fromList <$> [hiddenList, turnList]  -- sets for intersecting
    halves = (Set.size $ Set.intersection hiddenSet turnSet) - wholes

allCombisNoRep :: [Combi]
allCombisNoRep = let allCombisWithRep = mapM (const (enumFrom (toEnum 0) :: [Dig])) [1..4]
  in map (\[d1, d2, d3, d4] -> Combi d1 d2 d3 d4) . filter ((4==) . length . nub) $ allCombisWithRep

recursiveSolve :: [(Combi, Res)] -> [Combi] -> (Combi -> Res) -> [(Combi, Res)]
recursiveSolve observations possibileCombis eval =
  let chosenCombi:remainingCombis = filter (fitsObservations observations) possibileCombis
      -- ^ weighing should go here, instead of selecting the first
  in case (chosenCombi, eval chosenCombi) of
      o@(_, Res 4 0) -> o : observations
      o              -> recursiveSolve (o : observations) remainingCombis eval
  where
    fitsObservations :: [(Combi, Res)] -> Combi -> Bool
    fitsObservations obs c = all (id) . map (\(obC, obRes) -> evalWithHidden c obC == obRes) $ obs

solve :: (Combi -> Res) -> [(Combi, Res)]
solve = recursiveSolve [] allCombisNoRep


main :: IO()
main = do
  let allCombisNoRepReverse = reverse allCombisNoRep  -- try with and without reverse: quite a difference
  putStrLn $ "Average over 800/" ++ show (length allCombisNoRep) ++ " of a reversed valid combi list:"
  putStrLn . show . average . map (length . solve . evalWithHidden) . take 800 $ allCombisNoRepReverse
  putStrLn "\nSome attempts:"
  mapM_ (printObs . solve . evalWithHidden) allCombisNoRepReverse
  where
    printObs :: [(Combi, Res)] -> IO ()
    printObs os = putStrLn . (showTries os ++) . concat . map showObservation $ os
    
    showTries :: [a] -> String
    showTries = (++ ":  ") . show . length
    
    showObservation :: (Combi, Res) -> String
    showObservation (c, r) = show c ++ " -> " ++ show r ++ ".  "
    
    average :: [Int] -> Float
    average xs =
      let sumAndCount xs = foldr (\x (s, c) -> (s + x, c + 1)) (0, 0) xs
          (total, counter) = sumAndCount xs
      in (fromIntegral total) / (fromIntegral counter)
    


evalPossibleWH :: Int -> Int -> [(Int, Int)]
evalPossibleWH w h = [(pw, ph) | pw <- list, ph <- list, pw * 2 + ph == w * 2 + h, pw + ph <= 4]
  where
    list = [0,1,2,3,4]
