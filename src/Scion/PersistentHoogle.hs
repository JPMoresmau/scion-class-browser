module Scion.PersistentHoogle
( query
, downloadData
, checkDatabase
, module Scion.PersistentHoogle.Types
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Scion.PersistentBrowser ()
import Scion.PersistentHoogle.Types
import Scion.PersistentHoogle.Instances.Json ()
import Scion.PersistentHoogle.Parser
import Scion.PersistentHoogle.Util
import System.Exit (ExitCode(..))
import System.Process
import Text.Parsec.Prim (runP)

query :: String -> SqlPersist IO [Result]
query q = do mpath <- liftIO $ findHoogleBinPath
             case mpath of
               Nothing   -> return []
               Just path -> do (exitCode, output, err) <- liftIO $ readProcessWithExitCode path [q] ""
                               case exitCode of
                                 ExitSuccess -> do let search = runP hoogleElements () "hoogle-output" (output)
                                                   case search of
                                                     Right result -> do dbResult <- result
                                                                        return dbResult
                                                     Left  _      -> return []
                                 _           -> do liftIO $ putStrLn err
                                                   return []

downloadData :: IO Bool
downloadData = do mpath <- findHoogleBinPath
                  case mpath of
                    Nothing   -> return False
                    Just path -> do putStrLn "Running hoogle data..."
                                    (ec, _, err) <- readProcessWithExitCode path ["data"] ""
                                    when (ec/= ExitSuccess) (putStrLn err)
                                    return (ec == ExitSuccess)

checkDatabase :: IO Bool
checkDatabase = do mpath <- findHoogleBinPath
                   case mpath of
                     Nothing   -> return False
                     Just path -> do (exitCode, _, _) <- readProcessWithExitCode path ["fmap"] ""
                                     return (exitCode == ExitSuccess)

