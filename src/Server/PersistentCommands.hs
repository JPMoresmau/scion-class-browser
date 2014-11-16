{-# LANGUAGE OverloadedStrings #-}

module Server.PersistentCommands where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.HashMap.Lazy as M
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import Database.Persist.Sqlite hiding (get)
import Scion.PersistentBrowser
import Scion.PersistentBrowser.Build
--import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Query
import Scion.PersistentBrowser.Util (logToStdout,getHoogleDir)
import qualified Scion.PersistentHoogle as H
import Language.Haskell.Packages
import System.Directory
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (nub)
import Data.Vector (fromList)


data Command = LoadLocalDatabase FilePath Bool (Maybe FilePath)
             | LoadHackageDatabase FilePath Bool
             | GetPackages CurrentDatabase
             | GetModules CurrentDatabase String
             | GetDeclarations CurrentDatabase String 
             | HoogleQuery FilePath CurrentDatabase String (Maybe FilePath)
             | HoogleDownloadData FilePath (Maybe FilePath)
             | HoogleCheckDatabase FilePath (Maybe FilePath)
             | HoogleInitDatabase FilePath Bool (Maybe FilePath)
             | GetDeclarationModules CurrentDatabase String
             | SetExtraHooglePath String
             | GetDeclarationsFromPrefix CurrentDatabase String 
             | Quit

data CurrentDatabase = AllPackages
                     | HackageDatabase
                     | LocalDatabase
                     | APackage DbPackageIdentifier

data BrowserState = BrowserState
                      { localDb         :: Maybe FilePath
                      , hackageDb       :: Maybe FilePath
                      , extraHooglePath :: Maybe String
                      }


initialState :: BrowserState
initialState = BrowserState Nothing Nothing Nothing --True True Nothing

useLocal :: CurrentDatabase -> Bool
useLocal HackageDatabase=False
useLocal _=True

useHackage :: CurrentDatabase -> Bool
useHackage LocalDatabase=False
useHackage _=True

filterPackage :: CurrentDatabase -> Maybe DbPackageIdentifier
filterPackage (APackage pkgId)=Just pkgId
filterPackage _ = Nothing

runWithState :: BrowserState -> CurrentDatabase -> (Maybe DbPackageIdentifier -> SQL [a]) -> IO [a]
runWithState (BrowserState lDb hDb _) cdb action =
  do 
     let filterPkg=filterPackage cdb
     localThings <- runWithState' (useLocal cdb) lDb (action filterPkg)
     hackageThings <- runWithState' (useHackage cdb) hDb (action filterPkg)
     return $ localThings ++ hackageThings

runWithState' :: Bool -> Maybe FilePath -> SQL[a] -> IO [ a]
runWithState' use mpath action = if use && isJust mpath
                                    then do let path = fromJust mpath
                                            runResourceT $ runLogging $ withSqliteConn (T.pack path) $ runSqlConn action
                                    else return []

runDb :: CurrentDatabase -> (Maybe DbPackageIdentifier -> SQL [a]) -> BrowserM [a]
runDb cdb action = 
   do 
      st <- get
      lift $ runWithState st cdb action

type BrowserM = StateT BrowserState IO

executeCommand :: Command -> BrowserM (Value, Bool)  -- Bool indicates if continue receiving commands
executeCommand (LoadLocalDatabase path rebuild msandbox) =
  do fileExists <- lift $ doesFileExist path
     let fileExists' = fileExists `seq` fileExists
     when rebuild $
          lift $ do runResourceT $ runLogging $ withSqliteConn (T.pack path) $ runSqlConn $ do
                         runMigration migrateAll
                         createIndexes
                    pkgInfos' <- getPkgInfos msandbox
                    let pkgInfos = concat $ map snd pkgInfos'
                    updateDatabase path pkgInfos
     if fileExists' || rebuild -- If the file already existed or was rebuilt
        then do modify (\s -> s { localDb = Just path })
                lift $ logToStdout "Local database loaded"
        else modify (\s -> s { localDb = Nothing })
     return (ok, True)   
executeCommand (LoadHackageDatabase path rebuild) =
  do fileExists <- lift $ doesFileExist path
     when (not fileExists || rebuild) $
          lift $ do when fileExists (removeFile path)
                    logToStdout "Rebuilding Hackage database"
                    runResourceT $ runLogging $ withSqliteConn (T.pack path) $ runSqlConn $ do
                        runMigration migrateAll
                        createIndexes
                    saveHackageDatabase path
     if fileExists || rebuild -- If the file already existed or was rebuilt
        then do modify (\s -> s { hackageDb = Just path })
                lift $ logToStdout "Hackage database loaded"
        else modify (\s -> s { hackageDb = Nothing })
     return (ok, True)   
executeCommand (GetPackages cdb)         = do pkgs <- runDb cdb allPackages
                                              return (nubJSON pkgs, True)
executeCommand (GetModules cdb mname)  = 
                                           do smods <- runDb cdb (getSubmodules mname)
                                              return (nubJSON smods, True)
executeCommand (GetDeclarations cdb mname) = 
                                           do decls <- runDb cdb (getDeclsInModule mname)
                                              return (nubJSON decls, True)
executeCommand (GetDeclarationsFromPrefix cdb prefix) = 
                                           do decls <- runDb cdb (getDeclsFromPrefix prefix)
                                              return (nubJSON decls, True)
executeCommand (HoogleQuery path cdb query msandbox)       = do
  hoogleDir <- liftIO $ getHoogleDir path
  extraH <- fmap extraHooglePath get
  results <- runDb cdb (\_ -> H.query hoogleDir msandbox extraH query)
  return (nubJSON results, True)
executeCommand (HoogleDownloadData path msandbox)        = do 
  hoogleDir <- liftIO $ getHoogleDir path
  extraH <- fmap extraHooglePath get
  ret <- lift $ H.downloadData hoogleDir msandbox extraH
  return (String $ T.pack $ show ret, True)
executeCommand (HoogleCheckDatabase path msandbox) = do 
  hoogleDir <- liftIO $ getHoogleDir path
  extraH <- fmap extraHooglePath get
  ret <- lift $ H.checkDatabase hoogleDir msandbox extraH
  return (String $ T.pack $ show ret, True)
executeCommand (HoogleInitDatabase path addToDB msandbox) = do 
  extraH <- fmap extraHooglePath get
  ret <- lift $ H.initDatabase path msandbox extraH addToDB
  return (String $ T.pack $ show ret, True)
executeCommand (SetExtraHooglePath p)    = do modify (\s -> s { extraHooglePath = Just p })
                                              return (ok, True)
executeCommand (GetDeclarationModules cdb d) = 
                                           do mods <- runDb cdb (\_ -> getModulesWhereDeclarationIs d)
                                              return (toJSON mods, True)
executeCommand Quit                      = return (ok, False)

ok :: Value
ok = String "ok"

nubJSON :: (ToJSON a)=> [a] -> Value
nubJSON = Array . fromList . nub . map toJSON 

instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "load-local-db"     -> LoadLocalDatabase <$> v .: "filepath"
                                                                        <*> v .: "rebuild"
                                                                        <*> v .:? "sandbox"
                               "load-hackage-db"   -> LoadHackageDatabase <$> v .: "filepath"
                                                                          <*> v .: "rebuild"
                               "get-packages"      -> GetPackages <$> v .: "db"
                               "get-modules"       -> GetModules <$> v .: "db"
                                                                <*> v .: "module"
                               "get-declarations"  -> GetDeclarations <$>v .: "db"
                                                                <*> v .: "module"
                               "get-decl-prefix"   -> GetDeclarationsFromPrefix <$>v .: "db"
                                                                <*> v .: "prefix" 
                               "hoogle-query"      -> HoogleQuery <$> v .: "filepath"
                                                                  <*> v .: "db"
                                                                  <*> v .: "query"
                                                                  <*> v .:? "sandbox"
                               "hoogle-data"       -> HoogleDownloadData <$> v .: "filepath"
                                                                        <*> v .:? "sandbox"
                               "hoogle-check"      -> HoogleCheckDatabase <$> v .: "filepath"
                                                                        <*> v .:? "sandbox"
                               "hoogle-init"       -> HoogleInitDatabase <$> v .: "filepath"
                                                                        <*> v .:? "addToDB" .!= False
                                                                        <*> v .:? "sandbox"   
                                                                                                          
                               "extra-hoogle-path" -> SetExtraHooglePath <$> v .: "path"
                               "get-decl-module"   -> GetDeclarationModules <$> v .: "db"
                                                                <*> v .: "decl"
                               "quit"              -> pure Quit
                               _                   -> mzero
                           _ -> mzero
  parseJSON _          = mzero

instance FromJSON CurrentDatabase where
  parseJSON (String new) = case T.unpack new of
                             "_all"     -> pure AllPackages
                             "_hackage" -> pure HackageDatabase
                             "_local"   -> pure LocalDatabase
                             _          -> mzero
  parseJSON other        = APackage <$> parseJSON other
