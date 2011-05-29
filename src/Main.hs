{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scion.Browser
import Scion.Browser.Query
import qualified Data.Map as M

main :: IO ()
main = do -- 1. For creating the database
          -- saveHackageDatabase "hackage.db"
          -- 2. For loading the database
          maybeDb <- loadDatabase "hackage.db"
          case maybeDb of
            Nothing -> putStrLn "Error reading database"
            Just db -> mapM_ 
                         (\(Package doc pid _) -> do putStrLn $ show pid
                                                     putStrLn $ show doc) 
                         (map snd (M.toAscList db))
          putStrLn "Press a key to finish"
          getChar
          putStrLn "Finished"

