{-# LANGUAGE OverloadedStrings #-}

module Server.PersistentCommands where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson
import qualified Data.HashMap.Lazy as M
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import Database.Persist.Sqlite hiding (get)
import Scion.PersistentBrowser
import Scion.PersistentBrowser.Build
import Scion.PersistentBrowser.Query
import Scion.PersistentBrowser.Util (logToStdout)
import qualified Scion.PersistentHoogle as H
import Scion.Packages
import System.Directory

data Command = LoadLocalDatabase FilePath Bool
             | LoadHackageDatabase FilePath Bool
             | GetPackages
             | SetCurrentDatabase CurrentDatabase
             | GetModules String
             | GetDeclarations String
             | HoogleQuery String
             | HoogleDownloadData
             | HoogleCheckDatabase
             | GetDeclarationModules String
             | SetExtraHooglePath String
             | Quit

data CurrentDatabase = AllPackages
                     | HackageDatabase
                     | LocalDatabase
                     | APackage DbPackageIdentifier

data BrowserState = BrowserState
                      { localDb         :: Maybe FilePath
                      , hackageDb       :: Maybe FilePath
                      , useLocal        :: Bool
                      , useHackage      :: Bool
                      , filterPackage   :: Maybe DbPackageIdentifier
                      , extraHooglePath :: Maybe String
                      }

initialState :: BrowserState
initialState = BrowserState Nothing Nothing True True Nothing Nothing

runWithState :: BrowserState -> (Maybe DbPackageIdentifier -> SqlPersist IO [a]) -> IO [a]
runWithState (BrowserState lDb hDb useL useH filterPkg _) action =
  do localThings <- runWithState' useL lDb (action filterPkg)
     hackageThings <- runWithState' useH hDb (action filterPkg)
     return $ localThings ++ hackageThings

runWithState' :: Bool -> Maybe FilePath -> SqlPersist IO [a] -> IO [a]
runWithState' use mpath action = if use && isJust mpath
                                    then do let path = fromJust mpath
                                            withSqliteConn (T.pack path) $ runSqlConn action
                                    else return []

runDb :: (Maybe DbPackageIdentifier -> SqlPersist IO [a]) -> BrowserM [a]
runDb action = do st <- get
                  lift $ runWithState st action

type BrowserM = StateT BrowserState IO

executeCommand :: Command -> BrowserM (Value, Bool)  -- Bool indicates if continue receiving commands
executeCommand (LoadLocalDatabase path rebuild) =
  do fileExists <- lift $ doesFileExist path
     let fileExists' = fileExists `seq` fileExists
     when rebuild $
          lift $ do withSqliteConn (T.pack path) $ runSqlConn $ do
                         runMigration migrateAll
                         createIndexes
                    pkgInfos' <- getPkgInfos
                    let pkgInfos = concat $ map snd pkgInfos'
                    updateDatabase path pkgInfos
     if fileExists' || rebuild -- If the file already existed or was rebuilt
        then do modify (\s -> s { localDb = Just path })
                lift $ logToStdout "Local database loaded"
        else modify (\s -> s { hackageDb = Nothing })
     executeCommand (SetCurrentDatabase LocalDatabase)
executeCommand (LoadHackageDatabase path rebuild) =
  do fileExists <- lift $ doesFileExist path
     let fileExists' = fileExists `seq` fileExists
     when (not fileExists' || rebuild) $
          lift $ do when fileExists' (removeFile path)
                    logToStdout "Rebuilding Hackage database"
                    withSqliteConn (T.pack path) $ runSqlConn $ do
                        runMigration migrateAll
                        createIndexes
                    saveHackageDatabase path
     if fileExists' || rebuild -- If the file already existed or was rebuilt
        then do modify (\s -> s { hackageDb = Just path })
                lift $ withSqliteConn (T.pack path) $ runSqlConn $ createIndexes
                lift $ logToStdout "Hackage database loaded"
        else modify (\s -> s { hackageDb = Nothing })
     executeCommand (SetCurrentDatabase HackageDatabase)
executeCommand (SetCurrentDatabase db)  =
  do case db of
       AllPackages     -> do modify (\s -> s { useLocal = True,  useHackage = True,  filterPackage = Nothing })
                             return (String "ok", True)
       LocalDatabase   -> do modify (\s -> s { useLocal = True,  useHackage = False, filterPackage = Nothing })
                             return (String "ok", True)
       HackageDatabase -> do modify (\s -> s { useLocal = False, useHackage = True,  filterPackage = Nothing })
                             return (String "ok", True)
       APackage pid    -> do modify (\s -> s { useLocal = True,  useHackage = True,  filterPackage = Just pid })
                             return (String "ok", True)
executeCommand GetPackages               = do pkgs <- runDb allPackages
                                              return (toJSON pkgs, True)
executeCommand (GetModules mname)        = do smods <- runDb (getSubmodules mname)
                                              return (toJSON smods, True)
executeCommand (GetDeclarations mname)   = do decls <- runDb (getDeclsInModule mname)
                                              return (toJSON decls, True)
executeCommand (HoogleQuery query)       = do extraH <- fmap extraHooglePath get
                                              results <- runDb (\_ -> H.query extraH query)
                                              return (toJSON results, True)
executeCommand HoogleDownloadData        = do extraH <- fmap extraHooglePath get
                                              _ <- lift $ H.downloadData extraH
                                              return (String "ok", True)
executeCommand HoogleCheckDatabase       = do extraH <- fmap extraHooglePath get
                                              present <- lift $ H.checkDatabase extraH
                                              return (Bool present, True)
executeCommand (SetExtraHooglePath p)    = do modify (\s -> s { extraHooglePath = Just p })
                                              return (String "ok", True)
executeCommand (GetDeclarationModules d) = do mods <- runDb (\_ -> getModulesWhereDeclarationIs d)
                                              return (toJSON mods, True)
executeCommand Quit                      = return (String "ok", False)


instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "load-local-db"     -> LoadLocalDatabase <$> v .: "filepath"
                                                                        <*> v .: "rebuild"
                               "load-hackage-db"   -> LoadHackageDatabase <$> v .: "filepath"
                                                                          <*> v .: "rebuild"
                               "get-packages"      -> pure GetPackages
                               "set-current-db"    -> SetCurrentDatabase <$> v .: "new-db"
                               "get-modules"       -> GetModules <$> v .: "module"
                               "get-declarations"  -> GetDeclarations <$> v .: "module"
                               "hoogle-query"      -> HoogleQuery <$> v .: "query"
                               "hoogle-data"       -> pure HoogleDownloadData
                               "hoogle-check"      -> pure HoogleCheckDatabase
                               "extra-hoogle-path" -> SetExtraHooglePath <$> v .: "path"
                               "get-decl-module"   -> GetDeclarationModules <$> v .: "decl"
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

