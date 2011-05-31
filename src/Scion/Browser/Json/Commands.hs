module Scion.Browser.Json.Commands where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Distribution.Package hiding (Package)
import Scion.Browser
import Scion.Browser.Build (updateDatabase)
import Scion.Browser.Query
import Scion.Browser.Json.Instances ()
import Scion.Packages
import System.Directory

data Command = LoadLocalDatabase FilePath
             | GetPackages
             | SetCurrentDatabase CurrentDatabase
             | GetModules String
             | GetDeclarations String

data CurrentDatabase = AllPackages
                     | HackageDatabase
                     | LocalDatabase
                     | APackage PackageIdentifier

data BrowserState = BrowserState
                      { localDb   :: Database
                      , currentDb :: Database
                      }

initialState :: BrowserState
initialState = BrowserState M.empty M.empty

type BrowserM = StateT BrowserState IO

executeCommand :: Command -> BrowserM Value
executeCommand (LoadLocalDatabase path) =
  do fileExists <- lift $ doesFileExist path
     curDb <- (if not fileExists
                  then return M.empty
                  else do maybeDb <- lift $ loadDatabase path
                          case maybeDb of
                            Nothing    -> return M.empty
                            Just theDb -> return theDb)
     pkgInfos' <- lift $ getPkgInfos
     let pkgInfos = concat $ map snd pkgInfos'
     newDb <- lift $ updateDatabase curDb pkgInfos
     lift $ saveDatabase path newDb
     return $ String (T.pack "ok")
executeCommand (SetCurrentDatabase db)  =
  do case db of
       AllPackages   -> do modify (\s -> s { currentDb = localDb s })
                           return $ String (T.pack "ok")
       LocalDatabase -> do modify (\s -> s { currentDb = localDb s })
                           return $ String (T.pack "ok")
       APackage pid  -> do st <- get
                           case getSingletonDatabase pid (localDb st) of
                             Nothing    -> return $ String (T.pack "error")
                             Just newDb -> do modify (\s -> s { currentDb = newDb })
                                              return $ String (T.pack "ok")
       _             -> return $ String (T.pack "not implemented")
executeCommand GetPackages              = do db <- getCurrentDatabase
                                             return $ toJSON (allPackages db)
executeCommand (GetModules mname)       = do db <- getCurrentDatabase
                                             let smods = map fst (getModules mname db)
                                             return $ toJSON smods
executeCommand (GetDeclarations mname)  = do db <- getCurrentDatabase
                                             let decls = concat $ map snd (getDeclsInModule mname db)
                                             return $ toJSON decls

getCurrentDatabase :: BrowserM Database
getCurrentDatabase = do s <- get
                        return $ currentDb s

instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "load-local-db"    -> LoadLocalDatabase <$> v .: T.pack "filepath"
                               "get-packages"     -> pure GetPackages
                               "set-current-db"   -> SetCurrentDatabase <$> v .: T.pack "new-db"
                               "get-modules"      -> GetModules <$> v .: T.pack "module"
                               "get-declarations" -> GetDeclarations <$> v .: T.pack "module"
                               _                  -> mzero
                           _ -> mzero
  parseJSON _          = mzero

instance FromJSON CurrentDatabase where
  parseJSON (String new) = case T.unpack new of
                             "_all"     -> pure AllPackages
                             "_hackage" -> pure HackageDatabase
                             "_local"   -> pure LocalDatabase
                             _          -> mzero
  parseJSON other        = APackage <$> parseJSON other

