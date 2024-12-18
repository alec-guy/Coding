module Main where
import Control.Exception
import Types 
main :: IO ()
main = do 
    putStrLn  $ show $ mySquare 
    putStrLn $ show $ makeSquare (Position 1 0) 5
    putStrLn "1) move square up 1"
    putStrLn "2) move square down 1"
    putStrLn "3) move square right 1"
    putStrLn "4) move square left 1"
    choice <- catch ((readLn :: IO Int) >>= \i -> (return $ Just i )) $ \e -> do 
                 putStrLn $ show $ (e :: SomeException) 
                 return Nothing
    case choice of 
        Nothing -> main 
        (Just i) -> case i of 
                     1 -> putStrLn $ show $ moveSquareUp 1 mySquare
                     2 -> putStrLn $ show $ moveSquareDown 1 mySquare 
                     3 -> putStrLn $ show $ moveSquareRight 1 mySquare 
                     4 -> putStrLn $ show $ moveSquareLeft 1 mySquare 
                     _ -> main 
                         

