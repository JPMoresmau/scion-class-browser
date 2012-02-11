module Scion.PersistentHoogle.Types where

import Scion.PersistentBrowser.DbTypes

type Results = [Result]

data Result = RPackage     [DbPackage]
            | RModule      [(DbPackageIdentifier, DbModule)]
            | RDeclaration [(DbPackageIdentifier, String, DbCompleteDecl)]
            | RConstructor [(DbPackageIdentifier, String, DbCompleteDecl, DbConstructor)]
            | RKeyword     String

data Query = Query String

