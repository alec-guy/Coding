{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Web.Scotty 
import Types
import Data.Text as T
import Data.Text.Lazy

import Control.Exception hiding (catch)

main :: IO ()
main = do 
    stringHTML <- readFile "frontend/index.html"
    let textHTML = fromStrict $ T.pack stringHTML
    scotty 3000 $ do 
      get "/" $ do 
         html textHTML
      post "/popcorn" $ do 
        popcornNum <- catch (jsonData :: ActionM JsonPopcorn) $ \e -> do 
                         liftIO $ putStrLn $ show (e :: SomeException) 
                         return $ (JsonPopcorn "")
        liftIO $ putStrLn $  show $ popcornNum
        case popcornNum of 
         (JsonPopcorn "") -> return ()
         (JsonPopcorn s)  -> do 
                    let popcornBowl = fromPsuedoList $ Prelude.take (read s) pseudoPopcornBowl
                    json popcornBowl
