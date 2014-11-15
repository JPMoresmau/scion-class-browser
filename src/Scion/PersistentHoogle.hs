module Scion.PersistentHoogle
( query
, downloadData
, checkDatabase
, module Scion.PersistentHoogle.Types
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Scion.PersistentBrowser ()
import Scion.PersistentBrowser.Util
import Scion.PersistentBrowser.DbTypes

import Scion.PersistentHoogle.Types
import Scion.PersistentHoogle.Instances.Json ()
import Scion.PersistentHoogle.Parser
import Scion.PersistentHoogle.Util
import System.Exit (ExitCode(..))
import System.Process
import Text.Parsec.Prim (runP)

query :: Maybe FilePath -> Maybe FilePath -> String -> SQL [Result]
query msandbox p q = do 
  mpath <- liftIO $ findHoogleBinPath msandbox p
  case mpath of
   Nothing   -> return []
   Just path -> do (exitCode, output, err) <- liftIO $ readProcessWithExitCode path [q] ""
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

downloadData :: Maybe FilePath -> Maybe FilePath -> IO HoogleStatus
downloadData msandbox p = do 
  mpath <- findHoogleBinPath msandbox p
  case mpath of
    Nothing   -> return Missing
    Just path -> do logToStdout "Downloading hoogle data..."
                    (ec, _, err) <- readProcessWithExitCode path ["data"] ""
                    when (ec/= ExitSuccess) (do
                      logToStdout path
                      logToStdout err)
                    return $ case ec of
                      ExitSuccess->OK
                      _-> Error

checkDatabase :: Maybe FilePath -> Maybe FilePath -> IO HoogleStatus
checkDatabase msandbox p = do 
   mpath <- findHoogleBinPath msandbox p
   case mpath of
     Nothing   -> return Missing
     Just path -> do (ec, _, err) <- readProcessWithExitCode path ["fmap"] ""
                     when (ec/= ExitSuccess) (do
                       logToStdout path
                       logToStdout err)
                     return $ case ec of
                       ExitSuccess->OK
                       _-> Error

