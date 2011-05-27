{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scion.Browser
import Scion.Browser.Builder
import Distribution.Package hiding (Package)

main :: IO ()
main = do -- 1. For creating the database
          -- saveHackageDatabase "hackage.db"
          -- 2. For loading the database
          maybeDb <- loadDatabase "hackage.db"
          case maybeDb of
            Nothing -> putStrLn "Error reading database"
            Just db -> mapM_ putStrLn (map (\pkg -> let (PackageName n) = packageName pkg in n) db)

