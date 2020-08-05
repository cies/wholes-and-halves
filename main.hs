import qualified Data.Set as Set
import Data.List
import System.Random (randomRIO)
import Control.Monad (forM_)

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
  printObs observations
  putStrLn $ "possible combis: " ++ (intercalate ", " . map show $ possibleCombis)

  forM_ allCombisNoRep (\hc -> do
      let allPosibilities = allCombisNoRep
      tc <- pickRandElem allPosibilities
      let r = evalWithHidden hc tc
      printObs [(tc, r)]

      let allPosibilities2 = filter (fitsObservations [(tc, r)]) allPosibilities
      tc2 <- pickRandElem allPosibilities2
      let r2 = evalWithHidden hc tc
      printObs [(tc2, r2)]

      let allPosibilities3 = filter (fitsObservations [(tc, r), (tc2, r2)]) allPosibilities2
      tc3 <- pickRandElem allPosibilities3
      let r3 = evalWithHidden hc tc
      printObs [(tc3, r3)]

      let allPosibilities4 = filter (fitsObservations [(tc, r), (tc2, r2), (tc3, r3)]) allPosibilities3
      tc4 <- pickRandElem allPosibilities4
      let r4 = evalWithHidden hc tc
      printObs [(tc4, r4)]

      let allPosibilities5 = filter (fitsObservations [(tc, r), (tc2, r2), (tc3, r3), (tc4, r4)]) allPosibilities4
      tc5 <- pickRandElem allPosibilities5
      let r5 = evalWithHidden hc tc
      printObs [(tc5, r5)]

      let allPosibilities6 = filter (fitsObservations [(tc, r), (tc2, r2), (tc3, r3), (tc4, r4), (tc5, r5)]) allPosibilities5
      tc6 <- pickRandElem allPosibilities6
      let r6 = evalWithHidden hc tc
      printObs [(tc6, r6)]

      putStrLn "next... \n"
      )
  
  where
    printObs :: [(Combi, Res)] -> IO ()
    printObs os = mapM_ (\(c, r) -> putStrLn $ show c ++ " -> " ++ show r) os

    pickRandElem :: [a] -> IO a
    pickRandElem l = (l !!) <$> randomRIO (0, length l - 1)

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

    evalPossibleWH :: Int -> Int -> [(Int, Int)]
    evalPossibleWH w h = [(pw, ph) | pw <- list, ph <- list, pw * 2 + ph == w * 2 + h, pw + ph <= 4]
      where
        list = [0,1,2,3,4]
