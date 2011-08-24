{-# LANGUAGE OverloadedStrings #-}

module Server.Commands where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax hiding (String)
import Scion.Browser
import Scion.Browser.Build (updateDatabase)
import Scion.Browser.Query
import Scion.Browser.Util (logToStdout)
import qualified Scion.Hoogle as H
import Scion.Packages
import System.Directory

data Command = LoadLocalDatabase FilePath Bool
             | GetPackages
             | SetCurrentDatabase CurrentDatabase
             | GetModules String
             | GetDeclarations String
             | HoogleQuery String
             | HoogleDownloadData
             | GetDeclarationModules String
             | Quit

data CurrentDatabase = AllPackages
                     | HackageDatabase
                     | LocalDatabase
                     | APackage PackageIdentifier

data BrowserState = BrowserState
                      { allDb     :: Database
                      , localDb   :: Database
                      , currentDb :: Database
                      }

initialState :: BrowserState
initialState = BrowserState M.empty M.empty M.empty

type BrowserM = StateT BrowserState IO

executeCommand :: Command -> BrowserM (Value, Bool)  -- Bool indicates if continue receiving commands
executeCommand (LoadLocalDatabase path rebuild) =
  do fileExists <- lift $ doesFileExist path
     let fileExists' = fileExists `seq` fileExists
     curDb <- (if not fileExists'
                  then return M.empty
                  else do maybeDb <- lift $ loadDatabase path
                          case maybeDb of
                            Nothing    -> return M.empty
                            Just theDb -> return theDb)
     lift $ logToStdout "Database loaded"
     newDb <- if not rebuild
                 then return curDb
                 else do pkgInfos' <- lift $ getPkgInfos
                         let pkgInfos = concat $ map snd pkgInfos'
                         newDb <- lift $ updateDatabase curDb pkgInfos
                         lift $ logToStdout ("Saving on " ++ path)
                         lift $ saveDatabase path newDb
                         return newDb
     modify (\s -> s { localDb = newDb, allDb = newDb, currentDb = newDb })
     return (String "ok", True)
executeCommand (SetCurrentDatabase db)  =
  do case db of
       AllPackages   -> do modify (\s -> s { currentDb = localDb s })
                           return (String "ok", True)
       LocalDatabase -> do modify (\s -> s { currentDb = localDb s })
                           return (String "ok", True)
       APackage pid  -> do st <- get
                           case getSingletonDatabase pid (allDb st) of
                             Nothing    -> return (String "error", True)
                             Just newDb -> do modify (\s -> s { currentDb = newDb })
                                              return (String "ok", True)
       _             -> return (String (T.pack "not implemented"), True)
executeCommand GetPackages               = do db <- getCurrentDatabase
                                              return (toJSON (allPackages db), True)
executeCommand (GetModules mname)        = do db <- getCurrentDatabase
                                              let smods = getDocumentedModules (getSubmodules mname db)
                                              return (toJSON smods, True)
executeCommand (GetDeclarations mname)   = do db <- getCurrentDatabase
                                              -- let decls = concat $ map snd (getDeclsInModule mname db)
                                              let decls = getDeclsInModule mname db
                                              return (toJSON decls, True)
executeCommand (HoogleQuery query)       = do db <- getCurrentDatabase
                                              results <- lift $ H.query db query
                                              return (toJSON results, True)
executeCommand HoogleDownloadData        = do _ <- lift $ H.downloadData
                                              return (String "ok", True)
executeCommand (GetDeclarationModules d) = do db <- getCurrentDatabase
                                              let mods = getModulesWhereDeclarationIs d db
                                              return (toJSON mods, True)
executeCommand Quit                      = return (String "ok", False)

getCurrentDatabase :: BrowserM Database
getCurrentDatabase = do s <- get
                        return $ currentDb s

getEntireDatabase :: BrowserM Database
getEntireDatabase = do s <- get
                       return $ allDb s

getDocumentedModules :: [(String, [(PackageIdentifier, Documented Module)])] -> [Documented Module]
getDocumentedModules = map (getDocumentedModule . snd)

getDocumentedModule :: [(PackageIdentifier, Documented Module)] -> Documented Module
getDocumentedModule [(_, md)]                      = md
getDocumentedModule mds@((_, Module _ hd _ _ _):_) = let decls = concat $ map (\(Module _ _ _ _ decl) -> decl) $ map snd mds
                                                     in  Module NoDoc hd [] [] decls
getDocumentedModule _                              = error "The impossible happened"

instance FromJSON Command where
  parseJSON (Object v) = case M.lookup (T.pack "command") v of
                           Just (String e) ->
                             case T.unpack e of
                               "load-local-db"    -> LoadLocalDatabase <$> v .: "filepath"
                                                                       <*> v .: "rebuild"
                               "get-packages"     -> pure GetPackages
                               "set-current-db"   -> SetCurrentDatabase <$> v .: "new-db"
                               "get-modules"      -> GetModules <$> v .: "module"
                               "get-declarations" -> GetDeclarations <$> v .: "module"
                               "hoogle-query"     -> HoogleQuery <$> v .: "query"
                               "hoogle-data"      -> pure HoogleDownloadData
                               "get-decl-module"  -> GetDeclarationModules <$> v .: "decl"
                               "quit"             -> pure Quit
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

