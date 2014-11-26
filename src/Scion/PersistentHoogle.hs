module Scion.PersistentHoogle
( query
, downloadData
, checkDatabase
, initDatabase
, module Scion.PersistentHoogle.Types
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Scion.PersistentBrowser ()
import Scion.PersistentBrowser.Util
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Build
import Scion.PersistentBrowser.Types

import Scion.PersistentHoogle.Types
import Scion.PersistentHoogle.Instances.Json ()
import Scion.PersistentHoogle.Parser
import Scion.PersistentHoogle.Util
import System.Exit (ExitCode(..))
import System.Process
import System.Directory
import System.FilePath
import Text.Parsec.Prim (runP)
import Scion.PersistentBrowser.Parser (parseHoogleFile)
import Data.Maybe (catMaybes)
import Data.Either
import Scion.PersistentBrowser.ToDb (savePackageToDb, deletePackageByInfo)
import Scion.PersistentBrowser.FileUtil (withWorkingDirectory)

query :: FilePath -> Maybe FilePath -> Maybe FilePath -> String -> SQL [Result]
query hoogleDir msandbox p q = do 
  mpath <- liftIO $ findHoogleBinPath msandbox p
  case mpath of
   Nothing   -> return []
   Just path -> do -- use explicit search argument in case the search is itself a command, like 'data'
                   (exitCode, output, err) <- liftIO $ readProcessWithExitCode path ["search", q,"-d",hoogleDir] ""
                   case exitCode of
                     ExitSuccess -> do 
                                       liftIO $ logToStdout q
                                       liftIO $ logToStdout output
                                       let search = runP hoogleElements () "hoogle-output" output
                                       case search of
                                         Right result -> result
                                         Left  perr      -> do
                                          liftIO $ logToStdout $ show perr -- I like to see the error in the log
                                          return []
                     _           -> do liftIO $ logToStdout err -- I like to see the error in the log
                                       return []

downloadData :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO HoogleStatus
downloadData hoogleDir msandbox p = do 
  mpath <- findHoogleBinPath msandbox p
  case mpath of
    Nothing   -> return Missing
    Just path -> do logToStdout "Downloading hoogle data..."
                    (ec, _, err) <- readProcessWithExitCode path ["data","-d",hoogleDir] ""
                    when (ec/= ExitSuccess) (do
                      logToStdout path
                      logToStdout err)
                    return $ case ec of
                      ExitSuccess->OK
                      _-> Error

checkDatabase :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO HoogleStatus
checkDatabase hoogleDir msandbox p = do 
   mpath <- findHoogleBinPath msandbox p
   case mpath of
     Nothing   -> return Missing
     Just path -> do (ec, _, err) <- readProcessWithExitCode path ["fmap","-d",hoogleDir] ""
                     when (ec/= ExitSuccess) (do
                       logToStdout path
                       logToStdout err)
                     return $ case ec of
                       ExitSuccess->OK
                       _-> Error

-- | Init hoogle DB, adding extra files
initDatabase :: FilePath -> Maybe FilePath -> Maybe FilePath -> Bool-> IO HoogleStatus
initDatabase localDB msandbox p addToDb = do 
   hoogleDir <- getHoogleDir localDB
   mpath <- findHoogleBinPath msandbox p
   case mpath of
     Nothing   -> return Missing
     Just path -> 
       withWorkingDirectory hoogleDir $ do
         ok <- exec path ["fmap","-d",hoogleDir]
         unless ok (do
           exec path ["data","-d",hoogleDir]
           return ()
           ) 
         txts <- filter ((".txt" ==) . takeExtension) <$> getDirectoryContents hoogleDir
         docs <- forM txts $ \f -> do
           ret <- if addToDb 
            then Just <$> parseHoogleFile (hoogleDir </> f)
            else return Nothing
           ok1 <- exec path ["convert",hoogleDir </> f]
           when ok1 $ removeFile f
           return ret
         let (errors,okDocs) = partitionEithers $ catMaybes docs
         unless (null errors) $ logToStdout $ "addtoDB errors:" ++ show errors
         unless (null okDocs) $ runSQL localDB $ do
           mapM_ (deletePackageByInfo . (\ (Package _ pkgid _) -> pkgid)) okDocs
           mapM_ savePackageToDb okDocs
         hoos <- filter (("default.hoo" /=) . takeFileName) <$> filter ((".hoo" ==) . takeExtension) <$> getDirectoryContents hoogleDir
         unless (null hoos) $ do
           ok2 <- exec path ("combine":hoos)
           when ok2 $ forM_ hoos removeFile
         return OK

-- | Exec process and dump error
exec :: FilePath -> [String] -> IO Bool
exec path args= do
  (ec, _, err) <- readProcessWithExitCode path args ""
  when (ec/= ExitSuccess) (do
    logToStdout path
    logToStdout err)
  return $ case ec of
    ExitSuccess -> True
    _           -> False
