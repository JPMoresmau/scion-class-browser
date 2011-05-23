{-# LANGUAGE ScopedTypeVariables #-}

module Scion.PackageDB.Util where

import qualified Control.Exception as E
import Scion.PackageDB
import System.Exit (ExitCode)
import System.Process
import Text.Parsec.Error (ParseError)

executeCommand :: FilePath -> String -> [String] -> IO ExitCode
executeCommand tmp exe args =
  do let cproc = CreateProcess (RawCommand exe args)
                               (Just tmp)
                               Nothing
                               Inherit Inherit Inherit
                               True
     (_, _, _, h) <- createProcess cproc
     waitForProcess h

-- | Executes a set of actions and joins their
--   results. Actions which throw an exception
--   are not considered in the list.
mapMerr :: (a -> IO b) -> [a] -> IO [b]
mapMerr f []     = return []
mapMerr f (x:xs) = (do y  <- f x
                       ys <- mapMerr f xs
                       return (y:ys))
                   `E.catch` 
                   (\(_ :: E.SomeException) -> mapMerr f xs)

-- | Converts a list of parsed packages into a complete database,
--   and merges a list of errors.
partitionPackages :: [(FilePath, Either ParseError (Doc Package))] -> (Database, [(FilePath, ParseError)])
partitionPackages []                       = ([], [])
partitionPackages ((fname, Left error):xs) = let (db, errors) = partitionPackages xs
                                             in  (db, (fname, error):errors)
partitionPackages ((fname, Right pkg):xs)  = let (db, errors) = partitionPackages xs
                                             in  (pkg:db, errors)

-- | Takes out the "." and ".." special directory
--   entries from a list of file paths.
filterDots :: [FilePath] -> [FilePath]
filterDots = filter (\d -> d /= "." && d /= "..")

