{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, FlexibleInstances #-}

module Scion.PersistentBrowser.DbTypes where

import Scion.PersistentBrowser.Types

import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (LoggingT(..)) -- ,runStderrLoggingT
import Control.Monad.IO.Class (MonadIO)

type SQL a= SqlPersistT (LoggingT (ResourceT IO)) a

-- | wrapper around logging methods, so we can enable logging when we debug
runLogging :: MonadIO m => LoggingT m a -> m a
runLogging (LoggingT f) = f $ \_ _ _ _ -> return ()
        -- runStderrLoggingT

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
DbPackage
    name      String
    version   String
    doc       String Maybe
    UniqueVersion name version
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

