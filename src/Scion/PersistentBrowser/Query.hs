module Scion.PersistentBrowser.Query
( allPackageIds
, allPackages
, packagesByName
, getPackage
, getSubmodules
, getDeclsInModule
, getModulesWhereDeclarationIs
) where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Base
import Database.Persist.Sqlite
import Database.Persist.GenericSql.Raw (withStmt)
import Database.Persist.GenericSql.Internal (RowPopper)
import Scion.PersistentBrowser.DbTypes

-- |Get the identifiers of all packages in the database.
allPackageIds :: SqlPersist IO [DbPackageIdentifier]
allPackageIds = do packages <- allPackages
                   return $ map dbPackageToIdentifier packages

-- |Get information of all packages in the database.
allPackages :: SqlPersist IO [DbPackage]
allPackages = do packages <- selectList ([] :: [Filter DbPackage]) []
                 return $ map snd packages

-- |Get information of all versions of the package with that name.
packagesByName :: String -> SqlPersist IO [DbPackage]
packagesByName name = do packages <- selectList [ DbPackageName ==. name ] []
                         return $ map snd packages

-- |Get information about a package in the database.
getPackage :: DbPackageIdentifier -> SqlPersist IO (Maybe (DbPackage))
getPackage (DbPackageIdentifier name version) = do package <- selectFirst [ DbPackageName ==. name, DbPackageVersion ==. version ] []
                                                   return $ fmap snd package

-- |Get all the modules hierarchically inside the specified one.
--  For getting the entire list of modules modules, use "" as initial name.
getSubmodules :: String -> SqlPersist IO [DbModule]
getSubmodules modName = do let sql = "SELECT id FROM DbModule WHERE name LIKE '" ++ modName ++ ".%'"
                           modIds <- withStmt (T.pack sql) [] withPopper
                           mapM (\modId -> do Just md <- get (Key modId)
                                              return md) modIds

withPopper :: RowPopper (SqlPersist IO) -> SqlPersist IO [PersistValue]
withPopper popper = loop []
  where loop list = do mrow <- popper
                       case mrow of
                         Nothing        -> return list
                         Just [ modId ] -> loop (modId:list)
                         _              -> error "This should not happen"

-- |Gets the declarations inside some module,
--  along with information about which package it lives.
getDeclsInModule :: String -> SqlPersist IO [(DbPackage, [DbCompleteDecl])]
getDeclsInModule modName =
  do mods <- selectList [ DbModuleName ==. modName ] []
     mapM (\(modId, (DbModule _ _ packageId))->
             do decls <- selectList [ DbDeclModuleId ==. modId ] []
                declsAll <- mapM getAllDeclInfo decls
                Just package <- get packageId
                return (package, declsAll) ) mods

getAllDeclInfo :: (DbDeclId, DbDecl) -> SqlPersist IO DbCompleteDecl
getAllDeclInfo (declId, decl) =
  do ctxs' <- selectList [ DbContextDeclId ==. declId] []
     let ctxs = map snd ctxs'
     tyvars' <- selectList [ DbTyVarDeclId ==. declId] []
     let tyvars = map snd tyvars'
     fundeps' <- selectList [ DbFunDepDeclId ==. declId] []
     let fundeps = map snd fundeps'
     consts' <- selectList [ DbConstructorDeclId ==. declId] []
     let consts = map snd consts'
     return $ DbCompleteDecl decl ctxs tyvars fundeps consts

-- | Gets a list of modules where a declaration may live
getModulesWhereDeclarationIs :: String -> SqlPersist IO [DbModule]
getModulesWhereDeclarationIs declName =
  do decls' <- selectList [ DbDeclName ==. declName ] []
     let decls = map snd decls'
     cons <- selectList [ DbConstructorName ==. declName ] []
     consDecls <- mapM (\(_, (DbConstructor _ _ declId)) -> do Just decl <- get declId
                                                               return decl) cons
     mapM (\(DbDecl _ _ _ _ _ _ modId) -> do Just md <- get modId
                                             return md)
          (decls ++ consDecls)

