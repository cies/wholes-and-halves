import qualified Data.Set as Set
import Data.IORef
import Control.Monad
import System.Random

data Dig = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Show, Ord, Enum, Bounded)

data Ans = A0 | A1 | A2 | A3 | A4
  deriving (Eq, Show, Ord, Enum, Bounded)

data Pos = P1 | P2 | P3 | P4
  deriving (Eq, Show, Ord, Enum, Bounded)

data Combi = Combi Dig Dig Dig Dig
  deriving (Eq, Show)

data Res = Res Int Int
  deriving (Eq, Show)

evalWith :: Combi -> Combi -> Res
evalWith (Combi h1 h2 h3 h4) (Combi t1 t2 t3 t4) = Res wholes halves
  where
    hiddenList = [h1, h2, h3, h4]
    turnList = [t1, t2, t3, t4]
    wholes = foldl (\a p -> case p of { True -> a + 1 ; False -> a }) 0 $
        zipWith (==) turnList hiddenList
    hiddenSet = Set.fromList hiddenList
    turnSet = Set.fromList turnList
    halves = (Set.size $ Set.intersection hiddenSet turnSet) - wholes

main :: IO()
main = do
  let all = digList
  left <- newIORef all
  forM_ (posList) (\p -> do
    l <- readIORef left
    i <- randomRIO (0, length l - 1)
    let pick = l !! i
    modifyIORef left (deleteAt i)
    print (p, pick)
    )
  print . show $ evalWith (Combi D1 D2 D3 D4) (Combi D3 D4 D5 D1)
  where

    generateEnums :: (Enum a) => [a]
    generateEnums = enumFrom (toEnum 0)

    digList :: [Dig]
    digList = generateEnums

    posList :: [Pos]
    posList = generateEnums

    deleteAt :: Int -> [a] -> [a]
    deleteAt _ [] = []
    deleteAt n xs = take n xs ++ (drop $ n + 1) xs
