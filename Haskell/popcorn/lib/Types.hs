module Types where 
import System.Random 

data Popcorn = Unpopped 
             | HalfPoped 
             | Popped
             deriving (Show , Eq, Ord)

data Bowl a  = Single a (Bowl a)
             | Air
             deriving (Show , Eq)

fromList :: [Int] -> Bowl Popcorn
fromList [] = Air 
fromList (x : xs) = Single (toPopcorn x) (fromList xs)
toPopcorn :: Int -> Popcorn 

toPopcorn i  
    | i < 2 = Unpopped 
    | i < 6 = HalfPoped 
    | otherwise = Popped 

pseudoPopcornBowl :: [Int]
pseudoPopcornBowl = randomRs (0, 15) (mkStdGen 2024)




