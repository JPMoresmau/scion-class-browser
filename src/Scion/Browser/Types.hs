{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Scion.Browser.Types where

import Data.List (find, intersperse)
import qualified Data.Map as M
import qualified Data.Text as T
import Distribution.Package hiding (Package)
import qualified Distribution.Package as P
import Language.Haskell.Exts.Annotated.Syntax

-- |Documentation for an item.
-- Now it is simply a Text element.
data Doc = NoDoc
         | Doc T.Text
         deriving Show

docFromString :: String -> Doc
docFromString s = Doc (T.pack s)

-- |A documented item.
type Documented a = a Doc

-- |A package.
data Package l = Package l
                         PackageIdentifier
                         (M.Map String (Documented Module))
               deriving Show

instance P.Package (Package l) where
  packageId (Package _ i _) = i

-- |A Database saves a list of packages. 
type Database = M.Map PackageIdentifier (Documented Package)


-- |Gets the name inside a Name constructor.
getNameString :: Name l -> String
getNameString (Ident _ s)  = s
getNameString (Symbol _ s) = "(" ++ s ++ ")"

-- |Gets the qualified name as a string.
getQNameString :: QName l -> String
getQNameString (Qual _ (ModuleName _ "")    ename) = getNameString ename
getQNameString (Qual _ (ModuleName _ mname) ename) = mname ++ "." ++ getNameString ename
getQNameString (UnQual _ ename)                    = getNameString ename
getQNameString (Special _ (UnitCon _))             = "()"
getQNameString (Special _ (ListCon _))             = "[]"
getQNameString (Special _ (FunCon _))              = "(->)"
getQNameString (Special _ (TupleCon _ box n))      = case box of
                                                       Boxed   -> "(" ++ replicate (n-1) ',' ++ ")"
                                                       Unboxed -> "(#" ++ replicate (n-1) ',' ++ "#)"
getQNameString (Special _ (Cons _))                = "(:)"
getQNameString (Special _ (UnboxedSingleCon _))    = "(# #)"

-- ------------------------------
-- Datatypes for traversing docs.
-- ------------------------------

class Annotated e => Named e where
  getName :: Show l => (e l) -> String

class (Named parent, Named child) => DocItem parent child | parent -> child where
  getChildren :: Show l => (parent l) -> [child l]
  getChild :: Show l => (parent l) -> String -> Maybe (child l)
  getChild p name = find (\d -> (getName d) == name) (getChildren p)

instance Named Module where
  getName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = name
  getName v                                                              = error $ "This should not be possible: " ++ show v

instance DocItem Module Decl where
  getChildren (Module _ _ _ _ decls) = decls
  getChildren _                      = []

instance Named Decl where
  getName (TypeDecl _ (DHead _ name _) _)          = getNameString name
  getName (GDataDecl _ _ _ (DHead _ name _) _ _ _) = getNameString name
  getName (ClassDecl _ _ (DHead _ name _) _ _)     = getNameString name
  getName (InstDecl _ _ (IHead _ name _) _)        = getQNameString name
  getName (TypeSig _ name _)                       = concat $ intersperse "," $ map getNameString name
  getName v                                        = error $ "This should not be possible: " ++ show v

instance DocItem Decl GadtDecl where
  getChildren (GDataDecl _ _ _ _ _ cons _) = cons
  getChildren _                            = []

instance Named GadtDecl where
  getName (GadtDecl _ name _) = getNameString name

