{-# LANGUAGE TypeSynonymInstances, OverloadedStrings , FlexibleInstances #-}
-- FlexibleInstances needed for GHC 7.2

module Scion.PersistentBrowser.Query where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.Store
import Database.Persist.GenericSql.Raw (withStmt, execute)
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Util (logToStdout)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL

-- |Get the identifiers of all packages in the database.
allPackageIds :: Maybe DbPackageIdentifier -> SqlPersist IO [DbPackageIdentifier]
allPackageIds pkgs = do packages <- allPackages pkgs
                        return $ map dbPackageToIdentifier packages

-- |Get information of all packages in the database.
allPackages :: Maybe DbPackageIdentifier -> SqlPersist IO [DbPackage]
allPackages _ = do packages <- selectList ([] :: [Filter DbPackage]) []
                   return $ map entityVal packages

-- |Get information of all versions of the package with that name.
packagesByName :: String -> Maybe DbPackageIdentifier -> SqlPersist IO [DbPackage]
packagesByName name _ = do packages <- selectList [ DbPackageName ==. name ] []
                           return $ map entityVal packages

-- |Get information about a package in the database.
getPackage :: DbPackageIdentifier -> SqlPersist IO (Maybe (DbPackage))
getPackage (DbPackageIdentifier name version) = do package <- selectFirst [ DbPackageName ==. name, DbPackageVersion ==. version ] []
                                                   return $ fmap entityVal package

-- |Get information about all modules with that name.
modulesByName :: String -> Maybe DbPackageIdentifier -> SqlPersist IO [DbModule]
modulesByName name Nothing = do mods <- selectList [ DbModuleName ==. name ] []
                                return $ map entityVal mods
modulesByName name (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DbModule.name, DbModule.doc, DbModule.packageId FROM DbModule, DbPackage"
               ++ " WHERE DbModule.packageId = DbPackage.id "
               ++ " AND DbModule.name = ?"
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [name, pkgName, pkgVersion] moduleAction

-- |Get all the modules hierarchically inside the specified one.
--  For getting the entire list of modules modules, use "" as initial name.
getSubmodules :: String -> Maybe DbPackageIdentifier -> SqlPersist IO [DbModule]
getSubmodules "" Nothing =
  do let sql = "SELECT name, doc, packageId FROM DbModule"
     queryDb sql [] moduleAction
getSubmodules "" (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DbModule.name, DbModule.doc, DbModule.packageId FROM DbModule, DbPackage"
               ++ " WHERE DbModule.packageId = DbPackage.id "
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [pkgName, pkgVersion] moduleAction
getSubmodules modName Nothing =
  do let sql = "SELECT name, doc, packageId FROM DbModule WHERE name LIKE ?"
     queryDb sql [modName ++ ".%"] moduleAction
getSubmodules modName (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DbModule.name, DbModule.doc, DbModule.packageId FROM DbModule, DbPackage"
               ++ " WHERE name LIKE ?"
               ++ " AND DbModule.packageId = DbPackage.id "
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [modName ++ ".%", pkgName, pkgVersion] moduleAction

moduleAction :: [PersistValue] -> DbModule
moduleAction [PersistText name, doc, pkgId@(PersistInt64 _)] = DbModule (T.unpack name) (fromDbText doc) (Key pkgId)
moduleAction _ = error "This should not happen"

-- |Get information about all declaration with that name.
declsByName :: String -> Maybe DbPackageIdentifier -> SqlPersist IO [DbDecl]
declsByName name Nothing =
  do let sql = "SELECT DbDecl.declType, DbDecl.name, DbDecl.doc, DbDecl.kind, DbDecl.signature, DbDecl.equals, DbDecl.moduleId"
               ++ " FROM DbDecl, DbModule"
               ++ " WHERE DbModule.name = ?"
     queryDb sql [name] declAction
declsByName name (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DbDecl.declType, DbDecl.name, DbDecl.doc, DbDecl.kind, DbDecl.signature, DbDecl.equals, DbDecl.moduleId"
               ++ " FROM DbDecl, DbModule, DbPackage"
               ++ " WHERE DbDecl.moduleId = DbModule.id AND DbModule.packageId = DbPackage.id"
               ++ " AND DbModule.name = ?"
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [name, pkgName, pkgVersion] declAction

declAction :: [PersistValue] -> DbDecl
declAction [PersistText declType, PersistText name , doc, kind, signature, equals, modId@(PersistInt64 _)] =
  DbDecl (read (T.unpack declType)) (T.unpack name) (fromDbText doc)
         (fromDbText kind) (fromDbText signature) (fromDbText equals)
         (Key modId)
declAction _ = error "This should not happen"


createIndexes :: SqlPersist IO()
createIndexes=do
        liftIO $ logToStdout "creating indexes"
        let idxs = [ "create index if not exists module_pkgid_name on DbModule (packageId,name)"
                   , "create index if not exists decl_modid on DbDecl (moduleId)"
                   , "create index if not exists decl_name on DbDecl (name)"
                   , "create index if not exists cons_name on DbConstructor (name)"
                   , "create index if not exists cons_declid on DbConstructor (declId)"
                   , "create index if not exists tyvar_declid on DbTyVar (declId)"
                   , "create index if not exists fundep_declid on DbFunDep (declId)"
                   , "create index if not exists context_declid on DbContext (declId)"
                   ]
        mapM_ (\x -> execute x []) idxs
        execute "analyze" []

-- |Gets the declarations inside some module,
--  along with information about which package it lives.
getDeclsInModule :: String -> Maybe DbPackageIdentifier -> SqlPersist IO [(DbPackageIdentifier, DbCompleteDecl)]
getDeclsInModule modName pkgId =
  do let pkg = case pkgId of
                 Nothing -> ""
                 Just _ -> " AND DbPackage.name = ? AND DbPackage.version = ?"
     let sql = "SELECT DbDecl.id, DbDecl.declType, DbDecl.name, DbDecl.doc, DbDecl.kind, DbDecl.signature, DbDecl.equals, DbDecl.moduleId"
               ++ ", DbPackage.name, DbPackage.version"
               ++ " FROM DbDecl, DbModule, DbPackage"
               ++ " WHERE DbDecl.moduleId = DbModule.id AND DbModule.packageId = DbPackage.id"
               ++ " AND DbModule.name = ?"
               ++ pkg
     let args = case pkgId of
                  Nothing -> [modName]
                  Just (DbPackageIdentifier pkgName pkgVersion) -> [modName, pkgName, pkgVersion]
     elts <- queryDb sql args action
     completeElts <- mapM (\(dclId, dcl, p) -> do dclAll <- getAllDeclInfo (dclId, dcl)
                                                  return (p, dclAll)) elts
     return completeElts
  where action :: [PersistValue] -> (DbDeclId, DbDecl, DbPackageIdentifier)
        action [declId@(PersistInt64 _), PersistText declType, PersistText name
               , doc, kind, signature, equals, modId@(PersistInt64 _)
               , PersistText pkgName, PersistText pkgVersion] =
                                                                ( Key declId
                                                                , DbDecl (read (T.unpack declType)) (T.unpack name) (fromDbText doc)
                                                                         (fromDbText kind) (fromDbText signature) (fromDbText equals)
                                                                         (Key modId)
                                                                , DbPackageIdentifier (T.unpack pkgName) (T.unpack pkgVersion)
                                                                )
        action _ = error "This should not happen"

getAllDeclInfo :: (DbDeclId, DbDecl) -> SqlPersist IO DbCompleteDecl
getAllDeclInfo (declId, decl) =
  do ctxs' <- selectList [ DbContextDeclId ==. declId] []
     let ctxs = map entityVal ctxs'
     tyvars' <- selectList [ DbTyVarDeclId ==. declId] []
     let tyvars = map entityVal tyvars'
     fundeps' <- selectList [ DbFunDepDeclId ==. declId] []
     let fundeps = map entityVal fundeps'
     consts' <- selectList [ DbConstructorDeclId ==. declId] []
     let consts = map entityVal consts'
     return $ DbCompleteDecl decl ctxs tyvars fundeps consts

-- |Get information about all constructors with that name.
constructorsByName :: String -> SqlPersist IO [DbConstructor]
constructorsByName name = do consts <- selectList [ DbConstructorName ==. name ] []
                             return $ map entityVal consts

-- | Gets a list of modules where a declaration may live
getModulesWhereDeclarationIs :: String -> SqlPersist IO [DbModule]
getModulesWhereDeclarationIs declName =
  do let sqlDecl = "SELECT DbModule.name, DbModule.doc, DbModule.packageId"
                   ++ " FROM DbDecl, DbModule"
                   ++ " WHERE DbDecl.moduleId = DbModule.id AND DbDecl.name = ?"
         sqlCons = "SELECT DbModule.name, DbModule.doc, DbModule.packageId"
                   ++ " FROM DbConstructor, DbDecl, DbModule"
                   ++ " WHERE DbConstructor.declId = DbDecl.id AND DbDecl.moduleId = DbModule.id"
                   ++ " AND DbConstructor.name = ?"
     decls <- queryDb sqlDecl [declName] action
     cons <- queryDb sqlCons [declName] action
     return (decls ++ cons)
  where action :: [PersistValue] -> DbModule
        action [PersistText name, doc, pkgId@(PersistInt64 _)] = DbModule (T.unpack name) (fromDbText doc) (Key pkgId)
        action _ = error "This should not happen"

-- |Executes a query.
queryDb :: String -> [String] -> ([PersistValue] -> a) -> SqlPersist IO [a]
queryDb sql params action = runResourceT (withStmt (T.pack sql) (map toPersistValue params) $= CL.map action $$ CL.consume)

-- |Gets information from a text value.
fromDbText :: PersistValue -> Maybe String
fromDbText (PersistText value) = Just (T.unpack value)
fromDbText PersistNull         = Nothing
fromDbText _                   = error "This should not happen"

-- |Things that reside on a package.
class HasDbPackage d where
  getDbPackage :: d -> SqlPersist IO DbPackage

instance HasDbPackage DbPackage where
  getDbPackage = return

instance HasDbPackage DbModule where
  getDbPackage (DbModule _ _ pkgId) = do Just pkg <- get pkgId
                                         return pkg

instance HasDbPackage DbDecl where
  getDbPackage (DbDecl _ _ _ _ _ _ modId) = do Just md <- get modId
                                               getDbPackage md

-- |Things that reside on a module.
class HasDbModule d where
  getDbModule :: d -> SqlPersist IO DbModule

instance HasDbModule DbModule where
  getDbModule = return

instance HasDbModule DbDecl where
  getDbModule (DbDecl _ _ _ _ _ _ modId) = do Just md <- get modId
                                              return md

instance HasDbModule DbConstructor where
  getDbModule (DbConstructor _ _ declId) = do Just dc <- get declId
                                              getDbModule dc

