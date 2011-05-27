{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scion.Browser
import Scion.Browser.Builder
import Distribution.Package hiding (Package)
import qualified Data.Map as M

main :: IO ()
main = do -- 1. For creating the database
          -- saveHackageDatabase "hackage.db"
          -- 2. For loading the database
          maybeDb <- loadMDatabase "hackage.db"
          case maybeDb of
            Nothing -> putStrLn "Error reading database"
            Just db -> mapM_ putStrLn 
                         (map (show . packageId . fst)
                              (M.toAscList db))
          putStrLn "Press a key to finish"
          getChar
          putStrLn "Finished"

