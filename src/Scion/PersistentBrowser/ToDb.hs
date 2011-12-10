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
saveDeclToDb moduleId (GDataDecl doc (DataType _) ctx hd kind decls _) =
  do let (declName, declVars) = declHeadToDb hd
     declId <- insert $ DbDecl DbData declName (docToString doc) (fmap singleLinePrettyPrint kind) Nothing Nothing Nothing moduleId
     mapM_ (saveTyVarToDb declId) declVars

-- saveTyVarToDb :: PersistBackend backend m => DbDeclId -> String -> backend m ()
saveTyVarToDb declId var = insert $ DbTyVar var declId

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

