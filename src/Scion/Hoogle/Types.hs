module Scion.Hoogle.Types where

import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser

type Results = [Result]

data Result = RPackage     [Documented Package]
            | RModule      [(PackageIdentifier, Documented Module)]
            | RDeclaration [(PackageIdentifier, String, Documented Decl)]
            | RConstructor [(PackageIdentifier, String, Documented Decl, Documented GadtDecl)]

data Query = Query String

