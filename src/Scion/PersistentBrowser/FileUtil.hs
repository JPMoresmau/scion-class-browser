{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Scion.PersistentBrowser.FileUtil where

import Control.Applicative
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (bracket)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import System.Directory
import System.FilePath
import Scion.PersistentBrowser.TempFile
import Network.HTTP.Conduit

-- |Takes out the "." and ".." special directory
-- entries from a list of file paths.
filterDots :: [FilePath] -> [FilePath]
filterDots = filter (\d -> d /= "." && d /= "..")

-- |Downloads a file from the internet.
downloadFileLazy :: String -> IO LBS.ByteString
downloadFileLazy url = simpleHttp url

-- |Downloads a file from the internet.
downloadFileStrict :: String -> IO SBS.ByteString
downloadFileStrict = (LBS.toStrict <$>) . downloadFileLazy
                        

-- |Downloads a file from the internet and check it's a Hoogle file.
downloadHoogleFile :: Manager -> String -> IO (Maybe SBS.ByteString)
downloadHoogleFile mgr url = do
  req <- parseUrl url
  getHoogleFile <$> LBS.toStrict <$> responseBody <$> httpLbs req mgr
  


--downloadHoogleFile' :: String -> BrowserAction (HandleStream String) (Maybe SBS.ByteString)
--downloadHoogleFile' url = do 
--                            (_,res) <- request $ getRequest url
--                            return $ getHoogleFile $ rspBody res

getHoogleFile :: SBS.ByteString -> Maybe SBS.ByteString
getHoogleFile response=if "-- Hoogle documentation" `SBS.isPrefixOf` response
                                               then Just  response
                                               else Nothing

-- |Downloads a file from the internet, using the system proxy
--fetchURL :: String -> IO (String)
--fetchURL url=do
--            pr<-fetchProxy False
--            (_,res) <- browse $ do 
--                setErrHandler logToStdout
--                setOutHandler logToStdout
--                setProxy pr 
--                request $ getRequest url
--            return $ rspBody res
                            
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
-- the directory will be created inside the system temporary directory (cf bug 3413186)
-- the temporary directory will be automatically removed afterwards.
-- your working directory is not altered
withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory f =
     do home <- getTemporaryDirectory 
        bracket (createTempDirectory home ".scion")
                removeDirectoryRecursive
                f

