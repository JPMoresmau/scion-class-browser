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

executeCommand :: Command -> BrowserM Value
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
     return $ String (T.pack "ok")
executeCommand (SetCurrentDatabase db)  =
  do case db of
       AllPackages   -> do modify (\s -> s { currentDb = localDb s })
                           return $ String (T.pack "ok")
       LocalDatabase -> do modify (\s -> s { currentDb = localDb s })
                           return $ String (T.pack "ok")
       APackage pid  -> do st <- get
                           case getSingletonDatabase pid (allDb st) of
                             Nothing    -> return $ String (T.pack "error")
                             Just newDb -> do modify (\s -> s { currentDb = newDb })
                                              return $ String (T.pack "ok")
       _             -> return $ String (T.pack "not implemented")
executeCommand GetPackages              = do db <- getCurrentDatabase
                                             return $ toJSON (allPackages db)
executeCommand (GetModules mname)       = do db <- getCurrentDatabase
                                             let smods = getDocumentedModules (getSubmodules mname db)
                                             return $ toJSON smods
executeCommand (GetDeclarations mname)  = do db <- getCurrentDatabase
                                             -- let decls = concat $ map snd (getDeclsInModule mname db)
                                             let decls = getDeclsInModule mname db
                                             return $ toJSON decls
executeCommand (HoogleQuery query)      = do db <- getCurrentDatabase
                                             results <- lift $ H.query db query
                                             return $ toJSON results
executeCommand HoogleDownloadData       = do _ <- lift $ H.downloadData
                                             return $ String (T.pack "ok")

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
                               "load-local-db"    -> LoadLocalDatabase <$> v .: T.pack "filepath"
                                                                       <*> v .: T.pack "rebuild"
                               "get-packages"     -> pure GetPackages
                               "set-current-db"   -> SetCurrentDatabase <$> v .: T.pack "new-db"
                               "get-modules"      -> GetModules <$> v .: T.pack "module"
                               "get-declarations" -> GetDeclarations <$> v .: T.pack "module"
                               "hoogle-query"     -> HoogleQuery <$> v .: T.pack "query"
                               "hoogle-data"      -> pure HoogleDownloadData
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

