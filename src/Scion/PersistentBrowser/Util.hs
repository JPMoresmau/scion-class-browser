{-# LANGUAGE CPP #-}
module Scion.PersistentBrowser.Util where

import Control.Concurrent.ParallelIO.Local
import Scion.PersistentBrowser.Types
import System.Exit (ExitCode)
import System.IO (hFlush, stdout)
import System.Process
import Text.Parsec.Error (ParseError)
import GHC.Conc (numCapabilities)

withThreaded :: (Pool -> IO a) -> IO a
withThreaded = withPool numCapabilities

-- |Executes a command in a directory.
executeCommand :: FilePath     -- ^Working directory.
               -> String       -- ^Executable to run.
               -> [String]     -- ^Arguments.
               -> Bool         -- ^Show output
               -> IO ExitCode
executeCommand tmp exe args showOutput =
  do let cproc = CreateProcess (RawCommand exe args)
                               (Just tmp)
                               Nothing
                               Inherit 
                               (if showOutput then Inherit else CreatePipe)
                               (if showOutput then Inherit else CreatePipe)
                               True
#if __GLASGOW_HASKELL__ >= 702
                               False 
#endif
     (_, _, _, h) <- createProcess cproc
     waitForProcess h

-- |Converts a list of parsed packages into a complete database,
-- and merges a list of errors.
partitionPackages :: [(FilePath, Either ParseError (Documented Package))] -> ([Documented Package], [(FilePath, ParseError)])
partitionPackages []                     = ([], [])
partitionPackages ((fname, Left err):xs) = let (db, errors) = partitionPackages xs
                                           in  (db, (fname, err):errors)
partitionPackages ((_, Right pkg):xs)    = let (db, errors) = partitionPackages xs
                                           in  (pkg:db, errors)

logToStdout :: String -> IO ()
logToStdout msg = putStrLn msg >> hFlush stdout

