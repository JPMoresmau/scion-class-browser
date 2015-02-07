{-# LANGUAGE RankNTypes, KindSignatures, CPP, FunctionalDependencies, TypeFamilies #-}
module Scion.PersistentBrowser.ToDb where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.Persist
import Database.Persist.Sql
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax hiding (String)
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Types

-- SAVING IN THE DATABASE
-- ======================

savePackageToDb :: forall (m :: * -> *).
                     MonadIO m =>
                     Package Doc
                     -> ReaderT
                          SqlBackend m ()
savePackageToDb (Package doc (PackageIdentifier (PackageName name) version) modules) = 
  do pkgId <- insert $ DbPackage name (showVersion version) (docToString doc)
     mapM_ (saveModuleToDb pkgId) (M.elems modules)
  
saveModuleToDb :: forall (m :: * -> *).
                    MonadIO m =>
                    Key DbPackage -> Module Doc -> ReaderT SqlBackend m ()
saveModuleToDb pkgId (Module doc (Just (ModuleHead _ (ModuleName _ name)_ _)) _ _ decls) =
  do moduleId <- insert $ DbModule name (docToString doc) pkgId
     mapM_ (saveDeclToDb moduleId) decls
saveModuleToDb _ m = error $ "saveModuleToDb: This should never happen" ++ (show m)


saveDeclToDb :: forall (m :: * -> *).
                  MonadIO m =>
                  Key DbModule -> Decl Doc -> ReaderT SqlBackend m ()
saveDeclToDb moduleId (GDataDecl doc (DataType _) ctx hd kind decls _) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbData declName (docToString doc)
                               (fmap singleLinePrettyPrint kind) Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars
     mapM_ (saveContextToDb declId) (contextToDb (maybeEmptyContext ctx))
     mapM_ (saveConstructorToDb declId) decls
-- Newtypes
saveDeclToDb moduleId (GDataDecl doc (NewType _) ctx hd kind decls _) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbNewType declName (docToString doc)
                               (fmap singleLinePrettyPrint kind) Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars
     mapM_ (saveContextToDb declId) (contextToDb (maybeEmptyContext ctx))
     mapM_ (saveConstructorToDb declId) decls
-- Classes
saveDeclToDb moduleId (ClassDecl doc ctx hd fdeps _) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbClass declName (docToString doc)
                               Nothing Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars
     mapM_ (saveContextToDb declId) (contextToDb (maybeEmptyContext ctx))
     mapM_ (saveFunDepToDb declId) (map singleLinePrettyPrint fdeps)
-- Instances
#if MIN_VERSION_haskell_src_exts(1,16,0) 
saveDeclToDb moduleId (InstDecl doc _ hd _) =
  do let (declName, declVars) = instRuleToDb hd
         ctx = Nothing
#else
saveDeclToDb moduleId (InstDecl doc ctx hd _) =
  do let (declName, declVars) = instHeadToDb hd
#endif
     declId <- insert $ DbDecl DbInstance declName (docToString doc)
                               Nothing Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars
     mapM_ (saveContextToDb declId) (contextToDb (maybeEmptyContext ctx))
-- Signatures
saveDeclToDb moduleId (TypeSig doc names ty) =
  do mapM_ saveSignatureToDb names
  where saveSignatureToDb name = do
          insert $ DbDecl DbSignature (getNameString name) (docToString doc) 
                          Nothing (Just (singleLinePrettyPrint ty)) Nothing moduleId
-- Types
saveDeclToDb moduleId (TypeDecl doc hd ty) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbType declName (docToString doc) 
                               Nothing Nothing (Just (singleLinePrettyPrint ty)) moduleId
     mapM_ (saveTyVarToDb declId) declVars
-- Other
saveDeclToDb _ t = error $ "saveDeclToDb: This should never happen" ++ (show t)

saveTyVarToDb :: forall (m :: * -> *).
                   MonadIO m =>
                   Key DbDecl -> String -> ReaderT SqlBackend m (Key DbTyVar)
saveTyVarToDb declId var = insert $ DbTyVar var declId


saveFunDepToDb :: forall (m :: * -> *).
                    MonadIO m =>
                    Key DbDecl -> String -> ReaderT SqlBackend m (Key DbTyVar)
saveFunDepToDb declId var = insert $ DbTyVar var declId


saveContextToDb :: forall (m :: * -> *).
                     MonadIO m =>
                     Key DbDecl -> String -> ReaderT SqlBackend m (Key DbContext)
saveContextToDb declId ctx = insert $ DbContext ctx declId


saveConstructorToDb :: forall (m :: * -> *) l.
                         (SrcInfo l, MonadIO m) =>
                         Key DbDecl
                         -> GadtDecl l -> ReaderT SqlBackend m (Key DbConstructor)
#if MIN_VERSION_haskell_src_exts(1,16,0) 
saveConstructorToDb declId (GadtDecl _ name _ ty) = insert $ DbConstructor (getNameString name) (singleLinePrettyPrint ty) declId
#else
saveConstructorToDb declId (GadtDecl _ name ty) = insert $ DbConstructor (getNameString name) (singleLinePrettyPrint ty) declId
#endif
-- DELETE PACKAGE FROM DATABASE
-- ============================

deletePackageByInfo :: forall (m :: * -> *).
                         MonadIO m =>
                         PackageIdentifier -> ReaderT SqlBackend m ()
deletePackageByInfo (PackageIdentifier (PackageName name) version) =
  do Just pkg <- selectFirst [ DbPackageName ==. name, DbPackageVersion ==. showVersion version ] []
     let pkgId = entityKey pkg
     deletePackage pkgId

deletePackage :: forall (m :: * -> *).
                   MonadIO m =>
                   Key DbPackage -> ReaderT SqlBackend m ()
deletePackage pkgId =
  do modules <- selectList [ DbModulePackageId ==. pkgId ] []
     mapM_ (deleteModule . entityKey) modules
     delete pkgId

deleteModule :: forall (m :: * -> *).
                  MonadIO m =>
                  Key DbModule -> ReaderT SqlBackend m ()
deleteModule moduleId =
  do decls <- selectList [ DbDeclModuleId ==. moduleId ] []
     mapM_ (deleteDecl . entityKey) decls
     delete moduleId

deleteDecl :: forall (m :: * -> *).
                MonadIO m =>
                Key DbDecl -> ReaderT SqlBackend m ()
deleteDecl declId =
  do deleteWhere [ DbTyVarDeclId ==. declId ]
     deleteWhere [ DbFunDepDeclId ==. declId ]
     deleteWhere [ DbContextDeclId ==. declId ]
     deleteWhere [ DbConstructorDeclId ==. declId ]
     delete declId

-- UTILITIES FOR CONVERTING TO STRINGS
-- ===================================

docToString :: Doc -> Maybe String
docToString NoDoc     = Nothing
docToString (Doc doc) = Just (T.unpack doc)

declHeadToDb :: (Show l) => DeclHead l -> (String, [String])
#if MIN_VERSION_haskell_src_exts(1,16,0) 
declHeadToDb (DHead _ name) = (getNameString name, [])
declHeadToDb (DHApp _ h _) = declHeadToDb h
#else
declHeadToDb (DHead _ name vars) = (getNameString name, map singleLinePrettyPrint vars)
#endif
declHeadToDb h = error $ "declHeadToDb: This should never happen" ++ (show h)

#if MIN_VERSION_haskell_src_exts(1,16,0) 
instRuleToDb :: (Show l) => InstRule l -> (String, [String])
instRuleToDb (IRule _ mt _ (IHCon _ name )) = (getQNameString name, map singleLinePrettyPrint $ concat $ maybeToList mt)
instRuleToDb r = error $ "instRuleToDb: This should never happen" ++ (show r)
#endif

instHeadToDb :: (Show l) => InstHead l -> (String, [String])
#if MIN_VERSION_haskell_src_exts(1,16,0) 
instHeadToDb (IHCon _ name) = (getQNameString name, [])
#else
instHeadToDb (IHead _ name vars) = (getQNameString name, map singleLinePrettyPrint vars)
#endif
instHeadToDb h = error $ "instHeadToDb: This should never happen" ++ (show h)

#if MIN_VERSION_haskell_src_exts(1,16,0) 
instance SrcInfo Doc where
  toSrcInfo _ _ _ = NoDoc
  fromSrcInfo _ = NoDoc
  fileName = const "<unknown"
  startLine = const 0
  startColumn = const 0
#endif

singleLinePrettyPrint :: Pretty a => a -> String
singleLinePrettyPrint = prettyPrintWithMode $ defaultMode { layout = PPNoLayout }

maybeEmptyContext :: Maybe (Documented Context) -> Documented Context
maybeEmptyContext Nothing    = CxEmpty NoDoc
maybeEmptyContext (Just ctx) = ctx

#if MIN_VERSION_haskell_src_exts(1,16,0) 
contextToDb :: (SrcInfo l) => Context l -> [String]
#else
contextToDb :: Context l -> [String]
#endif
contextToDb (CxSingle _ a)  = [ singleLinePrettyPrint a ]
contextToDb (CxTuple _ as)  = map singleLinePrettyPrint as
#if !MIN_VERSION_haskell_src_exts(1,16,0) 
contextToDb (CxParen _ ctx) = contextToDb ctx
#endif
contextToDb (CxEmpty _)     = []

