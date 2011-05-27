module Scion.Browser.Util where

import Scion.Browser
import System.Exit (ExitCode)
import System.Process
import Text.Parsec.Error (ParseError)

-- |Executes a command in a directory.
executeCommand :: FilePath     -- ^Working directory.
               -> String       -- ^Executable to run.
               -> [String]     -- ^Arguments.
               -> IO ExitCode
executeCommand tmp exe args =
  do let cproc = CreateProcess (RawCommand exe args)
                               (Just tmp)
                               Nothing
                               Inherit Inherit Inherit
                               True
     (_, _, _, h) <- createProcess cproc
     waitForProcess h

-- |Converts a list of parsed packages into a complete database,
-- and merges a list of errors.
partitionPackages :: [(FilePath, Either ParseError (Documented Package))] -> ([Documented Package], [(FilePath, ParseError)])
partitionPackages []                       = ([], [])
partitionPackages ((fname, Left error):xs) = let (db, errors) = partitionPackages xs
                                             in  (db, (fname, error):errors)
partitionPackages ((fname, Right pkg):xs)  = let (db, errors) = partitionPackages xs
                                             in  (pkg:db, errors)

