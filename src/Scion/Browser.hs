{-# LANGUAGE TemplateHaskell #-}

module Scion.Browser where

import Control.Monad (liftM)
import Data.Serialize
import Data.DeriveTH
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Distribution.Package hiding (Package)
import qualified Distribution.Package as P
import Distribution.Version
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
type Database = [Documented Package]


-- Binary instances for different elements

instance Serialize T.Text where
  put = put . E.encodeUtf8
  get = liftM E.decodeUtf8 get

$( derive makeSerialize ''Doc )
$( derive makeSerialize ''Package )

-- derive Binary instances for Cabal packages
$( derive makeSerialize ''PackageIdentifier )
$( derive makeSerialize ''PackageName )
$( derive makeSerialize ''Version )

-- derive Binary instances for haskell-src-exts
$( derive makeSerialize ''Module )
$( derive makeSerialize ''ModuleHead )
$( derive makeSerialize ''WarningText )
$( derive makeSerialize ''ExportSpecList )
$( derive makeSerialize ''ExportSpec )
$( derive makeSerialize ''ImportDecl )
$( derive makeSerialize ''ImportSpecList )
$( derive makeSerialize ''ImportSpec )
$( derive makeSerialize ''Assoc )
$( derive makeSerialize ''Decl )
$( derive makeSerialize ''DeclHead )
$( derive makeSerialize ''InstHead )
$( derive makeSerialize ''Binds )
$( derive makeSerialize ''IPBind )
$( derive makeSerialize ''ClassDecl )
$( derive makeSerialize ''InstDecl )
$( derive makeSerialize ''Deriving )
$( derive makeSerialize ''DataOrNew )
$( derive makeSerialize ''ConDecl )
$( derive makeSerialize ''FieldDecl )
$( derive makeSerialize ''QualConDecl )
$( derive makeSerialize ''GadtDecl )
$( derive makeSerialize ''BangType )
$( derive makeSerialize ''Match )
$( derive makeSerialize ''Rhs )
$( derive makeSerialize ''GuardedRhs )
$( derive makeSerialize ''Context )
$( derive makeSerialize ''FunDep )
$( derive makeSerialize ''Asst )
$( derive makeSerialize ''Type )
$( derive makeSerialize ''Boxed )
$( derive makeSerialize ''Kind )
$( derive makeSerialize ''TyVarBind )
$( derive makeSerialize ''Exp )
$( derive makeSerialize ''Stmt )
$( derive makeSerialize ''QualStmt )
$( derive makeSerialize ''FieldUpdate )
$( derive makeSerialize ''Alt )
$( derive makeSerialize ''GuardedAlts )
$( derive makeSerialize ''GuardedAlt )
$( derive makeSerialize ''XAttr )
$( derive makeSerialize ''Pat )
$( derive makeSerialize ''PatField )
$( derive makeSerialize ''PXAttr )
$( derive makeSerialize ''RPat )
$( derive makeSerialize ''RPatOp )
$( derive makeSerialize ''Literal )
$( derive makeSerialize ''ModuleName )
$( derive makeSerialize ''QName )
$( derive makeSerialize ''Name )
$( derive makeSerialize ''QOp )
$( derive makeSerialize ''Op )
$( derive makeSerialize ''SpecialCon )
$( derive makeSerialize ''CName )
$( derive makeSerialize ''IPName )
$( derive makeSerialize ''XName )
$( derive makeSerialize ''Bracket )
$( derive makeSerialize ''Splice )
$( derive makeSerialize ''Safety )
$( derive makeSerialize ''CallConv )
$( derive makeSerialize ''ModulePragma )
$( derive makeSerialize ''Tool )
$( derive makeSerialize ''Rule )
$( derive makeSerialize ''RuleVar )
$( derive makeSerialize ''Activation )
$( derive makeSerialize ''Annotation )

