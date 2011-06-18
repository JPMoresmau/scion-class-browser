{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module Scion.Browser.FileUtil where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Network.HTTP
import System.FilePath

-- Taken from Unixutils.
import Control.Exception
import Data.List (isSuffixOf, isPrefixOf)
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import Foreign.C

-- |Takes out the "." and ".." special directory
-- entries from a list of file paths.
filterDots :: [FilePath] -> [FilePath]
filterDots = filter (\d -> d /= "." && d /= "..")

-- |Downloads a file from the internet.
downloadFileLazy :: String -> IO (Maybe LBS.ByteString)
downloadFileLazy url = do res <- simpleHTTP (getRequest url)
                          case res of
                            Left _  -> return Nothing
                            Right r -> let response = rspBody r
                                       in  return $ Just (LBS8.pack response)

-- |Downloads a file from the internet.
downloadFileStrict :: String -> IO (Maybe SBS.ByteString)
downloadFileStrict url = do res <- simpleHTTP (getRequest url)
                            case res of
                              Left _  -> return Nothing
                              Right r -> let response = rspBody r
                                         in  return $ Just (SBS8.pack response)

-- |Downloads a file from the internet and check it's a Hoogle file.
downloadHoogleFile :: String -> IO (Maybe SBS.ByteString)
downloadHoogleFile url = do res <- simpleHTTP (getRequest url)
                            case res of
                              Left _  -> return Nothing
                              Right r -> let response = rspBody r
                                         in if "-- Hoogle documentation" `isPrefixOf` response
                                               then return $ Just (SBS8.pack response)
                                               else return Nothing

-- |Un-gzip and un-tar a file into a folder.
unTarGzip :: LBS.ByteString -> FilePath -> IO ()
unTarGzip cnts folder = let ungzip  = GZip.decompress cnts
                            entries = Tar.read ungzip
                        in  do createDirectories entries
                               Tar.unpack folder entries
                        where createDirectories Tar.Done     = return ()
                              createDirectories (Tar.Fail _) = return ()
                              createDirectories (Tar.Next e es) =
                                case Tar.entryContent e of
                                  Tar.NormalFile _ _ -> do let dir = folder </> takeDirectory (Tar.entryPath e)
                                                           createDirectoryIfMissing True dir
                                                           createDirectories es
                                  Tar.Directory      -> do let dir = folder </> Tar.entryPath e
                                                           createDirectoryIfMissing True dir
                                                           createDirectories es
                                  _                  -> createDirectories es

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
removeRecursiveSafely pth =
    traverse pth removeFile removeDirectory umount
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
traverse pth f d m =
    do
      result <- try $ getSymbolicLinkStatus pth
      either (\ (_ :: SomeException) -> return ()) (doPath pth) result
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
          error ("Couldn't unmount file system on " ++ pth)
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

