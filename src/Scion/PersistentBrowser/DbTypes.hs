{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs,FlexibleContexts #-}

module Scion.PersistentBrowser.DbTypes where

import Database.Persist
import Database.Persist.Base
import Database.Persist.Sqlite
import Database.Persist.TH

data DbDeclType = DbData | DbNewType | DbClass | DbInstance | DbSignature | DbType
    deriving (Show, Read, Eq)
derivePersistField "DbDeclType"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
DbPackage
    name      String
    version   String
    doc       String Maybe
DbModule
    name      String
    doc       String Maybe
    packageId DbPackageId
DbDecl
    declType  DbDeclType
    name      String
    doc       String Maybe
    -- Depending on the type of decl,
    -- it will have some of these
    kind      String Maybe
    signature String Maybe
    equals    String Maybe
    moduleId  DbModuleId
DbTyVar
    name      String
    declId    DbDeclId
DbFunDep
    name      String
    declId    DbDeclId
DbContext
    shown     String
    declId    DbDeclId
DbConstructor
    name      String
    -- Called 'type' in Json output
    signature String
    declId    DbDeclId
|]

-- |Information needed to search a package.
data DbPackageIdentifier = DbPackageIdentifier String String -- name, version
     deriving Eq

dbPackageToIdentifier :: DbPackage -> DbPackageIdentifier
dbPackageToIdentifier (DbPackage name version _) = DbPackageIdentifier name version

-- |Complete information for a declaration.
--  Look at its ToJSON instance to know which one is used in each kind of declaration.
data DbCompleteDecl = DbCompleteDecl DbDecl [DbContext] [DbTyVar] [DbFunDep] [DbConstructor]

