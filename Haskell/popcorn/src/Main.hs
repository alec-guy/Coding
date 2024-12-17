module Main where

import Types 

main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    putStrLn "Infinite Popcorn Program"
    putStrLn "How many popcorn would you like from the infinite bowl?"
    numPop <- readLn :: IO Int
    putStrLn $ show $ fromList $ take numPop pseudoPopcornBowl
