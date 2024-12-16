module Main where

import Types
import System.IO 
import Text.Megaparsec 
import Text.Megaparsec.Error 
import Data.Char as C 
import Control.Monad (liftM2)
import Data.List ((\\), partition)
import Brick

main = do 
    putStrLn "Haskell Home Inventory Management System"
    pairs <- getPairs (pure [])
    let areas = case sortPairs (head pairs)  (pairs) of 
                 (Sorted areas') -> areas'
    simpleMain (vBox $ drawAreas areas)

------------------------------
data ViewPort = ViewPort deriving (Eq, Show, Ord)
drawAreas :: [Area] -> [Widget ViewPort] 
drawAreas areas = 
    case areas of 
        [] -> []
        ((Area items) : areas) -> (hBox $ drawPairs items) : (drawAreas areas)

drawPairs :: [(Item, Types.Location)] -> [Widget ViewPort] 
drawPairs items = 
    case items of 
        [] -> [] 
        (i : is) -> (vBox [drawPair i]) : (drawPairs is)

drawPair :: (Item, Types.Location) -> Widget ViewPort 
drawPair (item, location) = 
         hBox [str $ "item: " ++ (show item), str $ "   location: " ++ (show location)]
--------------------------------------------------
sortPairs :: (Item, Types.Location) -> [(Item, Types.Location)] -> Sorted 
sortPairs pair1 carryingbag = 
    case partition (equalLocations pair1) carryingbag of 
        (does, doesnot) -> 
            case null doesnot of 
                True  -> Sorted $ [Area does]
                False -> (Area $ if null does then [pair1] else does) `appendArea` (sortPairs (head doesnot) (doesnot \\ does))
                where appendArea :: Area -> Sorted -> Sorted 
                      appendArea (area) (Sorted l) = Sorted $ area : l

getItem :: IO Item 
getItem = do 
    maybeItem <- getLine 
    case parse parseItem "" maybeItem of 
        Left e -> do 
                   putStrLn $ errorBundlePretty e 
                   putStr $ "Enter Item: "
                   hFlush stdout
                   getItem 
        Right r -> return r

getLocation :: IO Types.Location
getLocation = do 
    maybeItem <- getLine 
    case parse parseLocation "" maybeItem of 
        Left e -> do 
                   putStrLn $ errorBundlePretty e 
                   putStr $ "Enter Item: "
                   hFlush stdout
                   getLocation
        Right r -> return r

getPairs :: IO [(Item,Types.Location)] -> IO [(Item, Types.Location)]
getPairs iol = do 
    putStr "Enter item: "
    hFlush stdout 
    item    <- getItem 
    putStr "Enter location: "
    hFlush stdout
    location <- getLocation
    putStr "stop (y/n): "
    hFlush stdout 
    c <- getLine 
    case C.toUpper <$> c of 
        "Y"   -> liftM2 (:) (return (item, location)) iol
        _     -> liftM2 (:) (return (item, location)) (getPairs iol)


