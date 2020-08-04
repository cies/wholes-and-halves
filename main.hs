import qualified Data.Set as Set
import Data.List

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
evalWithHidden (Combi h1 h2 h3 h4) (Combi t1 t2 t3 t4) = Res wholes halves
  where
    (hiddenList, turnList) = ([h1, h2, h3, h4], [t1, t2, t3, t4])   -- lists for zipping
    wholes = length . filter id $ zipWith (==) turnList hiddenList
    [hiddenSet, turnSet] = Set.fromList <$> [hiddenList, turnList]  -- sets for intersecting
    halves = (Set.size $ Set.intersection hiddenSet turnSet) - wholes

main :: IO()
main = do
  let observations =
          [ (Combi D1 D2 D3 D4, Res 0 2)
          , (Combi D1 D3 D2 D5, Res 0 2)
          , (Combi D9 D8 D1 D4, Res 1 1)
          ]
  let possibleCombis = filter (fitsObservations observations) allCombisNoRep

  putStrLn "observations: "
  mapM_ (\(c, r) -> putStrLn $ show c ++ " -> " ++ show r) observations
  putStrLn $ "possible combis: " ++ (intercalate ", " . map show $ possibleCombis)
  
  where
    generateEnums :: (Enum a) => [a]
    generateEnums = enumFrom (toEnum 0)

    digList :: [Dig]
    digList = generateEnums

    combiFromList :: [Dig] -> Combi
    combiFromList [d1, d2, d3, d4] = Combi d1 d2 d3 d4 

    allCombisNoRep :: [Combi]
    allCombisNoRep = map combiFromList . filter ((4==) . length . nub) $ mapM (const digList) [1..4]

    fitsObservations :: [(Combi, Res)] -> Combi -> Bool
    fitsObservations obs c = all (id) . map (\(obC, obRes) -> evalWithHidden c obC == obRes) $ obs


