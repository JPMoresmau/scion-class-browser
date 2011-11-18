{-# LANGUAGE RankNTypes, KindSignatures #-}
module Scion.PersistentBrowser.ToDb where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.Persist
import Database.Persist.Sqlite
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax hiding (String)
import Language.Haskell.Exts.Pretty
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Types

savePackageToDb :: Documented Package -> Connection -> IO ()
savePackageToDb (Package doc (PackageIdentifier (PackageName name) version) modules) = 
  runSqlConn $ do pkgId <- insert $ DbPackage name (showVersion version) (docToString doc)
                  mapM_ (saveModuleToDb pkgId) (M.elems modules)
  
-- saveModuleToDb :: PersistBackend backend m => DbPackageId -> Documented Module -> backend m ()
saveModuleToDb pkgId (Module doc (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ decls) =
  do moduleId <- insert $ DbModule name (docToString doc) pkgId
     mapM_ (saveDeclToDb moduleId) decls
saveModuleToDb _ _ = error "This should never happen"

-- saveDeclToDb :: PersistBackend backend m => DbModuleId -> Documented Decl -> backend m ()
-- Datatypes
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
saveDeclToDb moduleId (InstDecl doc ctx hd _) =
  do let (declName, declVars) = instHeadToDb hd
     declId <- insert $ DbDecl DbInstance declName (docToString doc)
                               Nothing Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars
     mapM_ (saveContextToDb declId) (contextToDb (maybeEmptyContext ctx))
-- Signatures
saveDeclToDb moduleId (TypeSig doc names ty) =
  do mapM_ saveSignatureToDb names
  where saveSignatureToDb name = 
          insert $ DbDecl DbSignature (getNameString name) (docToString doc) 
                          Nothing (Just (singleLinePrettyPrint ty)) Nothing moduleId
-- Types
saveDeclToDb moduleId (TypeDecl doc hd ty) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbType declName (docToString doc) 
                               Nothing Nothing (Just (singleLinePrettyPrint ty)) moduleId
     mapM_ (saveTyVarToDb declId) declVars

-- saveTyVarToDb :: PersistBackend backend m => DbDeclId -> String -> backend m ()
saveTyVarToDb declId var = insert $ DbTyVar var declId

-- saveFunDepToDb :: PersistBackend backend m => DbDeclId -> String -> backend m ()
saveFunDepToDb declId var = insert $ DbTyVar var declId

-- saveContextToDb :: PersistBackend backend m => DbDeclId -> String -> backend m ()
saveContextToDb declId ctx = insert $ DbContext ctx declId

-- saveConstructorToDb :: PersistBackend backend m => DbDeclId -> Documented GadtDecl -> backend m ()
saveConstructorToDb declId (GadtDecl _ name ty) = insert $ DbConstructor (getNameString name) (singleLinePrettyPrint ty) declId

docToString :: Doc -> Maybe String
docToString NoDoc     = Nothing
docToString (Doc doc) = Just (T.unpack doc)

declHeadToDb :: DeclHead l -> (String, [String])
declHeadToDb (DHead _ name vars) = (getNameString name, map singleLinePrettyPrint vars)
declHeadToDb _ = error "This should never happen"

instHeadToDb :: InstHead l -> (String, [String])
instHeadToDb (IHead _ name vars) = (getQNameString name, map singleLinePrettyPrint vars)
instHeadToDb _ = error "This should never happen"

singleLinePrettyPrint :: Pretty a => a -> String
singleLinePrettyPrint = prettyPrintWithMode $ defaultMode { layout = PPNoLayout }

maybeEmptyContext :: Maybe (Documented Context) -> Documented Context
maybeEmptyContext Nothing    = CxEmpty NoDoc
maybeEmptyContext (Just ctx) = ctx

contextToDb :: Context l -> [String]
contextToDb (CxSingle _ a)  = [ singleLinePrettyPrint a ]
contextToDb (CxTuple _ as)  = map singleLinePrettyPrint as
contextToDb (CxParen _ ctx) = contextToDb ctx
contextToDb (CxEmpty _)     = []

