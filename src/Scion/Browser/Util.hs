{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module Scion.Browser.Util where

import qualified Data.Map as M
import Distribution.Package (packageId)
import Scion.Browser
import System.Exit (ExitCode)
import System.Process
import Text.Parsec.Error (ParseError)

-- Taken from Unixutils.
import Control.Exception
import Data.List (isSuffixOf)
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Posix.Types
import Foreign.C

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

-- |Takes out the "." and ".." special directory
-- entries from a list of file paths.
filterDots :: [FilePath] -> [FilePath]
filterDots = filter (\d -> d /= "." && d /= "..")


-- HACK: Taken from Unixutils.
-- The version in Hackage conflicts with some of our packages.

-- |temporarily change the working directory to |dir| while running |action|
withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir action = 
    bracket getCurrentDirectory setCurrentDirectory (\ _ -> setCurrentDirectory dir >> action)

-- |create a temporary directory, run the action, remove the temporary directory
-- the first argument is a template for the temporary directory name
-- the directory will be created as a subdirectory of the directory returned by getTemporaryDirectory
-- the temporary directory will be automatically removed afterwards.
-- your working directory is not altered
withTemporaryDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryDirectory fp f =
     do sysTmpDir <- getTemporaryDirectory
        bracket (mkdtemp (sysTmpDir </> fp))
                removeRecursiveSafely
                f

foreign import ccall unsafe "stdlib.h mkdtemp"
  c_mkdtemp :: CString -> IO CString

mkdtemp :: FilePath -> IO FilePath
mkdtemp template = 
      withCString (if "XXXXXX" `isSuffixOf` template then template else (template ++ "XXXXXX")) $ \ ptr -> do
        cname <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
        name <- peekCString cname
        return name
-- |Recursively remove a directory contents on a single file system.
-- The adjective \"Safely\" refers to these features:
--   1. It will not follow symlinks
--   2. If it finds a directory that seems to be a mount point,
--	it will attempt to unmount it up to five times.  If it
--	still seems to be a mount point it gives up
--   3. It doesn't use /proc/mounts, which is ambiguous or wrong
--	when you are inside a chroot.
removeRecursiveSafely :: FilePath -> IO ()
removeRecursiveSafely path =
    traverse path removeFile removeDirectory umount
    where
      umount path =
          do
            hPutStrLn stderr ("-- removeRecursiveSafely: unmounting " ++ path)
            -- This is less likely to hang and more likely to succeed
            -- than regular umount.
            let cmd = "umount -l " ++ path
            result <- system cmd
            case result of
              ExitSuccess -> return ()
              ExitFailure n -> error ("Failure: " ++ cmd ++ " -> " ++ show n)

traverse :: FilePath -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> IO ()
-- ^ Traverse a file system directory applying D to every directory, F
-- to every non-directory file, and M to every mount point.
-- NOTE: It is tempting to use the "find" function to returns a list
-- of the elements of the directory and then map that list over an
-- "unmount and remove" function.  However, because we are unmounting
-- as we traverse, the contents of the file list may change in ways
-- that could confuse the find function.
traverse path f d m =
    do
      result <- try $ getSymbolicLinkStatus path
      either (\ (_ :: SomeException) -> return ()) (doPath path) result
    where
      doPath path status =
          if isDirectory status then
              do
                getDirectoryContents path >>= mapM (doDirectoryFile 1 status path)
                d path else
              f path

      doDirectoryFile :: Int -> FileStatus -> FilePath -> String -> IO ()
      doDirectoryFile _ _ _ "." = return ()
      doDirectoryFile _ _ _ ".." = return ()
      doDirectoryFile tries _ _ _ | tries >= 5 =
          error ("Couldn't unmount file system on " ++ path)
      doDirectoryFile tries status path name =
          do
            let child = path </> name
            childStatus <- getSymbolicLinkStatus child
            if deviceID status == deviceID childStatus then
                doPath child childStatus else
                do
                  if tries > 1 then hPutStrLn stderr ("try " ++ show tries ++ ":") else return ()
                  m child
                  doDirectoryFile (tries + 1) status path name

