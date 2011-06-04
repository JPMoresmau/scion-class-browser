module Scion.Hoogle.Converter where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser
import Scion.Browser.Query
import Scion.Hoogle.Parser
import Scion.Hoogle.Types

convertHalfToResult :: Database -> HalfResult -> Result
convertHalfToResult db (HalfPackage  pname)      = RPackage (packagesByName pname db)
convertHalfToResult db (HalfModule   mname _)    = RModule (findPackagesForModule db mname)
convertHalfToResult db (HalfDecl     mname decl) = undefined
convertHalfToResult db (HalfGadtDecl mname decl) = undefined

findPackagesForModule :: Database -> String -> [(PackageIdentifier, Documented Module)]
findPackagesForModule db md = let pkgs = M.filter (\(Package _ _ mds) -> M.member md mds) db
                              in  M.toAscList $ M.map (\(Package _ _ mds) -> fromJust $ M.lookup md mds) pkgs

findDeclsInModules :: [(PackageIdentifier, Documented Module)] -> String -> [(PackageIdentifier, String, Documented Decl)]
findDeclsInModules mods declName = undefined

