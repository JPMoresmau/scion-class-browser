{-# LANGUAGE TypeSynonymInstances, OverloadedStrings , FlexibleInstances #-}
-- FlexibleInstances needed for GHC 7.2

module Scion.PersistentBrowser.Query where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Scion.PersistentBrowser.DbTypes
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List (isPrefixOf)
import Data.Char (toUpper)

-- |Get the identifiers of all packages in the database.
allPackageIds :: Maybe DbPackageIdentifier -> SQL [DbPackageIdentifier]
allPackageIds pkgs = do packages <- allPackages pkgs
                        return $ map dbPackageToIdentifier packages

-- |Get information of all packages in the database.
allPackages :: Maybe DbPackageIdentifier -> SQL [DbPackage]
allPackages _ = do packages <- selectList ([] :: [Filter DbPackage]) []
                   return $ map entityVal packages

-- |Get information of all versions of the package with that name.
packagesByName :: String -> Maybe DbPackageIdentifier -> SQL [DbPackage]
packagesByName name _ = do packages <- selectList [ DbPackageName ==. name ] []
                           return $ map entityVal packages

-- |Get information about a package in the database.
getPackage :: DbPackageIdentifier -> SQL (Maybe DbPackage)
getPackage (DbPackageIdentifier name version) = do package <- selectFirst [ DbPackageName ==. name, DbPackageVersion ==. version ] []
                                                   return $ fmap entityVal package

-- |Get information about all modules with that name.
modulesByName :: String -> Maybe DbPackageIdentifier -> SQL [DbModule]
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
getSubmodules :: String -> Maybe DbPackageIdentifier -> SQL [DbModule]
getSubmodules "" Nothing =
  do let sql = "SELECT DISTINCT name, doc, packageId FROM DbModule"
     queryDb sql [] moduleAction
getSubmodules "" (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DISTINCT DbModule.name, DbModule.doc, DbModule.packageId FROM DbModule, DbPackage"
               ++ " WHERE DbModule.packageId = DbPackage.id "
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [pkgName, pkgVersion] moduleAction
getSubmodules modName Nothing =
  do let sql = "SELECT DISTINCT name, doc, packageId FROM DbModule WHERE name LIKE ?"
     queryDb sql [modName ++ ".%"] moduleAction
getSubmodules modName (Just (DbPackageIdentifier pkgName pkgVersion)) =
  do let sql = "SELECT DISTINCT DbModule.name, DbModule.doc, DbModule.packageId FROM DbModule, DbPackage"
               ++ " WHERE name LIKE ?"
               ++ " AND DbModule.packageId = DbPackage.id "
               ++ " AND DbPackage.name = ?"
               ++ " AND DbPackage.version = ?"
     queryDb sql [modName ++ ".%", pkgName, pkgVersion] moduleAction

moduleAction :: [PersistValue] -> DbModule
moduleAction [PersistText name, doc, pkgId@(PersistInt64 _)] = DbModule (T.unpack name) (fromDbText doc) (Key pkgId)
moduleAction _ = error "This should not happen"

-- |Get information about all declaration with that name.
declsByName :: String -> Maybe DbPackageIdentifier -> SQL [DbDecl]
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


createIndexes :: SQL ()
createIndexes=do
        -- liftIO $ logToStdout "creating indexes"
        let idxs = [ "create index if not exists module_pkgid_name on DbModule (packageId,name)"
                   , "create index if not exists decl_modid on DbDecl (moduleId)"
                   , "create index if not exists decl_name on DbDecl (name)"
                   , "create index if not exists cons_name on DbConstructor (name)"
                   , "create index if not exists cons_declid on DbConstructor (declId)"
                   , "create index if not exists tyvar_declid on DbTyVar (declId)"
                   , "create index if not exists fundep_declid on DbFunDep (declId)"
                   , "create index if not exists context_declid on DbContext (declId)"
                   ]
        mapM_ (\x -> rawExecute x []) idxs
        rawExecute "analyze" []

-- |Gets the declarations inside some module,
--  along with information about which package it lives.
getDeclsInModule :: String -> Maybe DbPackageIdentifier -> SQL [(DbPackageIdentifier, DbCompleteDecl)]
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
     mapM (\(dclId, dcl, p) -> do 
      dclAll <- getAllDeclInfo (dclId, dcl)
      return (p, dclAll)) elts
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

-- | list declarations matching the given prefix, useful for content assist
-- the prefix either matches the declaration itself or any constructor
getDeclsFromPrefix :: String -> Maybe DbPackageIdentifier -> SQL [(DbPackageIdentifier, DbModule, DbCompleteDecl)]
getDeclsFromPrefix prefix pkgId =
  do let pkg = case pkgId of
                 Nothing -> ""
                 Just _ -> " AND DbPackage.name = ? AND DbPackage.version = ?"
     let sql = "SELECT DbDecl.id, DbDecl.declType, DbDecl.name, DbDecl.doc, DbDecl.kind, DbDecl.signature, DbDecl.equals, DbDecl.moduleId, "
               ++ "DbModule.name, DbPackage.name, DbPackage.version"
               ++ " FROM DbDecl, DbModule, DbPackage"
               ++ " WHERE DbDecl.moduleId = DbModule.id AND DbModule.packageId = DbPackage.id"
               
               ++ (if null prefix then "" else " AND (DbDecl.name LIKE '"
               ++ prefix ++ "%' or DbDecl.id in (select DbConstructor.declId from DbConstructor where DbConstructor.name LIKE '"
               ++ prefix ++ "%'))")
               ++ pkg
     let args = case pkgId of
                  Nothing -> []
                  Just (DbPackageIdentifier pkgName pkgVersion) -> [pkgName, pkgVersion]
     elts <- queryDb sql args action
     mapM (\(dclId, dcl, p,m) -> do 
      cs <- consts dclId
      let dclAll=DbCompleteDecl dcl [] [] [] cs
      return (p,m, dclAll)) elts
  where action :: [PersistValue] -> (DbDeclId, DbDecl, DbPackageIdentifier, DbModule)
        action [declId@(PersistInt64 _), PersistText declType, PersistText name
               , doc, kind, signature, equals, modId@(PersistInt64 _)
               , PersistText modName, PersistText pkgName, PersistText pkgVersion] =
                                                                ( Key declId
                                                                , DbDecl (read (T.unpack declType)) (T.unpack name) (fromDbText doc)
                                                                         (fromDbText kind) (fromDbText signature) (fromDbText equals)
                                                                         (Key modId)
                                                                , DbPackageIdentifier (T.unpack pkgName) (T.unpack pkgVersion)
                                                                , DbModule (T.unpack modName) Nothing (Key modId)
                                                                )
        action _ = error "This should not happen"
        consts declId=do
                consts' <- selectList [ DbConstructorDeclId ==. declId] []
                -- we do case insensitive match here to be consistent with LIKE above
                return $ filter (\(DbConstructor name _ _)->isPrefixOf (map toUpper prefix) (map toUpper name)) $ map entityVal consts'

getAllDeclInfo :: (DbDeclId, DbDecl) -> SQL DbCompleteDecl
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
constructorsByName :: String -> SQL [DbConstructor]
constructorsByName name = do consts <- selectList [ DbConstructorName ==. name ] []
                             return $ map entityVal consts

-- | Gets a list of modules where a declaration may live
getModulesWhereDeclarationIs :: String -> SQL [(DbModule,String,String)]
getModulesWhereDeclarationIs declName =
  do let sqlDecl = "SELECT DbModule.name, DbModule.doc, DbModule.packageId,'',DbPackage.name"
                   ++ " FROM DbDecl, DbModule, DbPackage"
                   ++ " WHERE DbDecl.moduleId = DbModule.id AND DbDecl.name = ? AND DbPackage.id=DbModule.packageId" 
         sqlCons = "SELECT DbModule.name, DbModule.doc, DbModule.packageId,DbDecl.name,DbPackage.name"
                   ++ " FROM DbConstructor, DbDecl, DbModule, DbPackage"
                   ++ " WHERE DbConstructor.declId = DbDecl.id AND DbDecl.moduleId = DbModule.id"
                   ++ " AND DbConstructor.name = ? AND DbPackage.id=DbModule.packageId"
     decls <- queryDb sqlDecl [declName] action
     cons <- queryDb sqlCons [declName] action
     return (decls ++ cons)
  where action :: [PersistValue] -> (DbModule,String,String)
        action [PersistText name, doc, pkgId@(PersistInt64 _),PersistText decl,PersistText pkgName] = (DbModule (T.unpack name) (fromDbText doc) (Key pkgId),T.unpack decl,T.unpack pkgName)
        action _ = error "This should not happen"

-- |Executes a query.
queryDb :: String -> [String] -> ([PersistValue] -> a) -> SQL [a]
queryDb sql params action = rawQuery (T.pack sql) (map toPersistValue params) $= CL.map action
  $$ CL.consume


-- |Gets information from a text value.
fromDbText :: PersistValue -> Maybe String
fromDbText (PersistText value) = Just (T.unpack value)
fromDbText PersistNull         = Nothing
fromDbText _                   = error "This should not happen"

-- |Things that reside on a package.
class HasDbPackage d where
  getDbPackage :: d -> SQL DbPackage

instance HasDbPackage DbPackage where
  getDbPackage = return

instance HasDbPackage DbModule where
  getDbPackage (DbModule _ _ pkgId) = getJust pkgId

instance HasDbPackage DbDecl where
  getDbPackage (DbDecl _ _ _ _ _ _ modId) = do md <- getJust modId
                                               getDbPackage md

-- |Things that reside on a module.
class HasDbModule d where
  getDbModule :: d -> SQL DbModule

instance HasDbModule DbModule where
  getDbModule = return

instance HasDbModule DbDecl where
  getDbModule (DbDecl _ _ _ _ _ _ modId) = getJust modId

instance HasDbModule DbConstructor where
  getDbModule (DbConstructor _ _ declId) = do 
                                              dc <-  getJust declId
                                              getDbModule dc

