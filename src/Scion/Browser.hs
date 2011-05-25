{-# LANGUAGE TemplateHaskell #-}

module Scion.Browser where

import Control.Monad (liftM)
import Data.Binary
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

-- |A Database maps packages to their descriptions. 
type Database = M.Map PackageIdentifier (Documented Package)


-- Binary instances for different elements

instance Binary T.Text where
  put = put . E.encodeUtf8
  get = liftM E.decodeUtf8 get

$( derive makeBinary ''Doc )
$( derive makeBinary ''Package )

-- derive Binary instances for Cabal packages
$( derive makeBinary ''PackageIdentifier )
$( derive makeBinary ''PackageName )
$( derive makeBinary ''Version )

-- derive Binary instances for haskell-src-exts
$( derive makeBinary ''Module )
$( derive makeBinary ''ModuleHead )
$( derive makeBinary ''WarningText )
$( derive makeBinary ''ExportSpecList )
$( derive makeBinary ''ExportSpec )
$( derive makeBinary ''ImportDecl )
$( derive makeBinary ''ImportSpecList )
$( derive makeBinary ''ImportSpec )
$( derive makeBinary ''Assoc )
$( derive makeBinary ''Decl )
$( derive makeBinary ''DeclHead )
$( derive makeBinary ''InstHead )
$( derive makeBinary ''Binds )
$( derive makeBinary ''IPBind )
$( derive makeBinary ''ClassDecl )
$( derive makeBinary ''InstDecl )
$( derive makeBinary ''Deriving )
$( derive makeBinary ''DataOrNew )
$( derive makeBinary ''ConDecl )
$( derive makeBinary ''FieldDecl )
$( derive makeBinary ''QualConDecl )
$( derive makeBinary ''GadtDecl )
$( derive makeBinary ''BangType )
$( derive makeBinary ''Match )
$( derive makeBinary ''Rhs )
$( derive makeBinary ''GuardedRhs )
$( derive makeBinary ''Context )
$( derive makeBinary ''FunDep )
$( derive makeBinary ''Asst )
$( derive makeBinary ''Type )
$( derive makeBinary ''Boxed )
$( derive makeBinary ''Kind )
$( derive makeBinary ''TyVarBind )
$( derive makeBinary ''Exp )
$( derive makeBinary ''Stmt )
$( derive makeBinary ''QualStmt )
$( derive makeBinary ''FieldUpdate )
$( derive makeBinary ''Alt )
$( derive makeBinary ''GuardedAlts )
$( derive makeBinary ''GuardedAlt )
$( derive makeBinary ''XAttr )
$( derive makeBinary ''Pat )
$( derive makeBinary ''PatField )
$( derive makeBinary ''PXAttr )
$( derive makeBinary ''RPat )
$( derive makeBinary ''RPatOp )
$( derive makeBinary ''Literal )
$( derive makeBinary ''ModuleName )
$( derive makeBinary ''QName )
$( derive makeBinary ''Name )
$( derive makeBinary ''QOp )
$( derive makeBinary ''Op )
$( derive makeBinary ''SpecialCon )
$( derive makeBinary ''CName )
$( derive makeBinary ''IPName )
$( derive makeBinary ''XName )
$( derive makeBinary ''Bracket )
$( derive makeBinary ''Splice )
$( derive makeBinary ''Safety )
$( derive makeBinary ''CallConv )
$( derive makeBinary ''ModulePragma )
$( derive makeBinary ''Tool )
$( derive makeBinary ''Rule )
$( derive makeBinary ''RuleVar )
$( derive makeBinary ''Activation )
$( derive makeBinary ''Annotation )

