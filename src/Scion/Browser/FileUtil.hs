{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module Scion.Browser.FileUtil where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (bracket)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (isPrefixOf)
import Network.Browser
import Network.HTTP
import Network.HTTP.Proxy
import System.Directory
import System.FilePath
import Scion.Browser.TempFile

-- |Takes out the "." and ".." special directory
-- entries from a list of file paths.
filterDots :: [FilePath] -> [FilePath]
filterDots = filter (\d -> d /= "." && d /= "..")

-- |Downloads a file from the internet.
downloadFileLazy :: String -> IO (Maybe LBS.ByteString)
downloadFileLazy url = do 
                        response <- fetchURL url
                        return $ Just (LBS8.pack response)

-- |Downloads a file from the internet.
downloadFileStrict :: String -> IO (Maybe SBS.ByteString)
downloadFileStrict url = do 
                        response <- fetchURL url
                        return $ Just (SBS8.pack response)

-- |Downloads a file from the internet and check it's a Hoogle file.
downloadHoogleFile :: String -> IO (Maybe SBS.ByteString)
downloadHoogleFile url = do 
                            response <- fetchURL url
                            if "-- Hoogle documentation" `isPrefixOf` response
                                               then return $ Just (SBS8.pack response)
                                               else return Nothing

-- |Downloads a file from the internetn, using the system proxy
fetchURL :: String -> IO (String)
fetchURL url=do
            pr<-fetchProxy False
            (_,res) <- browse $ do 
                setProxy pr 
                request $ getRequest url
            return $ rspBody res
                            
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
-- the directory will be created as a subdirectory of the directory returned by getTemporaryDirectory
-- the temporary directory will be automatically removed afterwards.
-- your working directory is not altered
withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory f =
     do home <- getCurrentDirectory
        bracket (createTempDirectory home ".scion")
                removeDirectoryRecursive
                f

