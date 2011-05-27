{-# LANGUAGE TemplateHaskell #-}

module Scion.Browser where

import Control.DeepSeq
import Control.Monad (liftM)
import Data.Serialize
import Data.DeriveTH
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Distribution.Package hiding (Package)
import qualified Distribution.Package as P
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import System.IO

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


saveDatabase :: FilePath -> Database -> IO ()
saveDatabase fpath db  = withFile fpath WriteMode $
                           \hnd -> BS.hPut hnd (encode db)

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase fpath = withFile fpath ReadMode $
                       \hnd -> do s <- BS.hGetContents hnd
                                  return $ case decode s of
                                             Left _  -> Nothing
                                             Right p -> p `deepseq` Just p

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

-- for DeepSeq

$( derive makeNFData ''Doc )
$( derive makeNFData ''Package )

-- derive Binary instances for Cabal packages
$( derive makeNFData ''PackageIdentifier )
$( derive makeNFData ''PackageName )
$( derive makeNFData ''Version )

-- derive Binary instances for haskell-src-exts
$( derive makeNFData ''Module )
$( derive makeNFData ''ModuleHead )
$( derive makeNFData ''WarningText )
$( derive makeNFData ''ExportSpecList )
$( derive makeNFData ''ExportSpec )
$( derive makeNFData ''ImportDecl )
$( derive makeNFData ''ImportSpecList )
$( derive makeNFData ''ImportSpec )
$( derive makeNFData ''Assoc )
$( derive makeNFData ''Decl )
$( derive makeNFData ''DeclHead )
$( derive makeNFData ''InstHead )
$( derive makeNFData ''Binds )
$( derive makeNFData ''IPBind )
$( derive makeNFData ''ClassDecl )
$( derive makeNFData ''InstDecl )
$( derive makeNFData ''Deriving )
$( derive makeNFData ''DataOrNew )
$( derive makeNFData ''ConDecl )
$( derive makeNFData ''FieldDecl )
$( derive makeNFData ''QualConDecl )
$( derive makeNFData ''GadtDecl )
$( derive makeNFData ''BangType )
$( derive makeNFData ''Match )
$( derive makeNFData ''Rhs )
$( derive makeNFData ''GuardedRhs )
$( derive makeNFData ''Context )
$( derive makeNFData ''FunDep )
$( derive makeNFData ''Asst )
$( derive makeNFData ''Type )
$( derive makeNFData ''Boxed )
$( derive makeNFData ''Kind )
$( derive makeNFData ''TyVarBind )
$( derive makeNFData ''Exp )
$( derive makeNFData ''Stmt )
$( derive makeNFData ''QualStmt )
$( derive makeNFData ''FieldUpdate )
$( derive makeNFData ''Alt )
$( derive makeNFData ''GuardedAlts )
$( derive makeNFData ''GuardedAlt )
$( derive makeNFData ''XAttr )
$( derive makeNFData ''Pat )
$( derive makeNFData ''PatField )
$( derive makeNFData ''PXAttr )
$( derive makeNFData ''RPat )
$( derive makeNFData ''RPatOp )
$( derive makeNFData ''Literal )
$( derive makeNFData ''ModuleName )
$( derive makeNFData ''QName )
$( derive makeNFData ''Name )
$( derive makeNFData ''QOp )
$( derive makeNFData ''Op )
$( derive makeNFData ''SpecialCon )
$( derive makeNFData ''CName )
$( derive makeNFData ''IPName )
$( derive makeNFData ''XName )
$( derive makeNFData ''Bracket )
$( derive makeNFData ''Splice )
$( derive makeNFData ''Safety )
$( derive makeNFData ''CallConv )
$( derive makeNFData ''ModulePragma )
$( derive makeNFData ''Tool )
$( derive makeNFData ''Rule )
$( derive makeNFData ''RuleVar )
$( derive makeNFData ''Activation )
$( derive makeNFData ''Annotation )

