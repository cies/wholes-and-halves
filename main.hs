import           Data.List         (nub)
import           Control.Monad     (mapM_)


data Dig = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pos = P1 | P2 | P3 | P4
  deriving (Eq, Show, Ord, Enum, Bounded)

data Combi = Combi Dig Dig Dig Dig deriving (Eq)
instance Show Combi where
  show (Combi d1 d2 d3 d4) = concat $ drop 1 . show <$> [d1, d2, d3, d4]
class NumEq a where numEq :: a -> a -> Int
instance NumEq Combi where
    numEq (Combi x1 x2 x3 x4) (Combi y1 y2 y3 y4) =
        foldr (\e a -> if e then a + 1 else a) 0 $ [x1 == y1, x2 == y2, x3 == y3, x4 == y4]

data Res = Res Int Int deriving (Eq)
instance Show Res where
  show (Res wholes halves) = show wholes ++ "." ++ show halves
  -- show (Res wholes halves) = show wholes ++ "● " ++ show halves ++ "◐"

evalWithoutHidden :: Combi -> Combi -> Res
evalWithoutHidden hidden@(Combi h1 h2 h3 h4) turn@(Combi t1 t2 t3 t4) = Res wholes halves
  where
    wholes = numEq turn hidden
    halves = (length $ filter (`elem` [h1, h2, h3, h4]) [t1, t2, t3, t4]) - wholes

toCombi :: [Dig] -> Combi
toCombi [d1, d2, d3, d4] = Combi d1 d2 d3 d4

allDigListsWithRep :: [[Dig]]
allDigListsWithRep = mapM (const (enumFrom (toEnum 0) :: [Dig])) [1..4]

allCombisNoRep :: [Combi]
allCombisNoRep = map toCombi . filter ((4==) . length . nub) $ allDigListsWithRep

allCombisWithRep :: [Combi]
allCombisWithRep = map toCombi allDigListsWithRep

recursiveSolve :: [(Combi, Res)] -> [Combi] -> Combi -> (Combi -> Res) -> [(Combi, Res)]
recursiveSolve observations possibileCombis chosenCombi eval = case (chosenCombi, eval chosenCombi) of
      o@(_, Res 4 0) -> o : observations
      o              -> let newChosenCombi:remainingCombis = filterCombis o possibileCombis
                        in  recursiveSolve (o:observations) remainingCombis newChosenCombi eval
  where
    filterCombis :: (Combi, Res) -> [Combi] -> [Combi]
    filterCombis (obC, obRes) = filter (\c -> evalWithoutHidden c obC == obRes)

solve :: (Combi -> Res) -> [(Combi, Res)]
solve = recursiveSolve [] (tail allCombisNoRep) (head allCombisNoRep)


main :: IO ()
main = do
  -- putStrLn $ "Average over all " ++ show (length allCombisNoRep) ++ " of a valid combi list:"
  -- putStrLn . show . average . map (length . solve . evalWithoutHidden) $ allCombisNoRep
  putStrLn "\nSome attempts:"
  mapM_ (printObs . solve . evalWithoutHidden) allCombisNoRep
  where
    printObs :: [(Combi, Res)] -> IO ()
    printObs os = putStrLn . (showTries os ++) . concat . map showObservation $ os
    
    showTries :: [a] -> String
    showTries = (++ ":  ") . show . length
    
    showObservation :: (Combi, Res) -> String
    showObservation (c, r) = show c ++ " -> " ++ show r ++ "  "
    
    average :: [Int] -> Float
    average xs =
      let sumAndCount xs = foldr (\x (s, c) -> (s + x, c + 1)) (0, 0) xs
          (total, counter) = sumAndCount xs
      in (fromIntegral total) / (fromIntegral counter)
    


evalPossibleWH :: Int -> Int -> [(Int, Int)]
evalPossibleWH w h = [(pw, ph) | pw <- [0..4], ph <- [0..4], pw * 2 + ph == w * 2 + h, pw + ph <= 4]
