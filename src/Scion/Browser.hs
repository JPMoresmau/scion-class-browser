{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}

module Scion.Browser
( Package (..)
, Database
, saveDatabase
, loadDatabase
, singletonDatabase
, Named
, getName
, DocItem
, getChildren
, getChild
, module Scion.Browser.HSEInstances
) where

import Control.DeepSeq
import qualified Data.ByteString as BS
import Data.DeriveTH
import Data.List (find)
import qualified Data.Map as M
import Data.Serialize
import Distribution.Package hiding (Package)
import qualified Distribution.Package as P
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser.HSEInstances
import System.IO

-- |A package.
data Package l = Package l
                         PackageIdentifier
                         (M.Map String (Documented Module))
               deriving Show

instance P.Package (Package l) where
  packageId (Package _ i _) = i

-- |A Database saves a list of packages. 
type Database = M.Map PackageIdentifier (Documented Package)

saveDatabase :: FilePath -> Database -> IO ()
saveDatabase fpath db  = withFile fpath WriteMode $
                           \hnd -> do BS.hPut hnd (encode db)
                                      hFlush hnd

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase fpath = withFile fpath ReadMode $
                       \hnd -> do s <- BS.hGetContents hnd
                                  return $ case decode s of
                                             Left _  -> Nothing
                                             Right p -> p `deepseq` Just p

singletonDatabase :: Documented Package -> Database
singletonDatabase pkg@(Package _ pid _) = M.singleton pid pkg

-- ------------------------------
-- Datatypes for traversing docs.
-- ------------------------------

class Annotated e => Named e where
  getName :: (e l) -> String

class (Named parent, Named child) => DocItem parent child | parent -> child where
  getChildren :: (parent l) -> [child l]
  getChild :: (parent l) -> String -> Maybe (child l)
  getChild p name = find (\d -> (getName d) == name) (getChildren p)

instance Named Module where
  getName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = name
  getName _                                                              = ""

instance DocItem Module Decl where
  getChildren (Module _ _ _ _ decls) = decls
  getChildren _                      = []

instance Named Decl where
  getName (TypeDecl _ (DHead _ name _) _)          = getNameString name
  getName (GDataDecl _ _ _ (DHead _ name _) _ _ _) = getNameString name
  getName (ClassDecl _ _ (DHead _ name _) _ _)     = getNameString name
  getName (InstDecl _ _ (IHead _ name _) _)        = getQNameString name
  getName _                                        = ""

instance DocItem Decl GadtDecl where
  getChildren (GDataDecl _ _ _ _ _ cons _) = cons
  getChildren _                            = []

instance Named GadtDecl where
  getName (GadtDecl _ name _) = getNameString name

-- Serialize instances
$( derive makeSerialize ''Package )
$( derive makeSerialize ''PackageIdentifier )
$( derive makeSerialize ''PackageName )
$( derive makeSerialize ''Version )

-- NFData instances
$( derive makeNFData ''Package )
$( derive makeNFData ''PackageIdentifier )
$( derive makeNFData ''PackageName )
$( derive makeNFData ''Version )

