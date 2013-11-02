module Scion.PersistentHoogle.Types where

import Scion.PersistentBrowser.DbTypes

type Results = [Result]

data Result = RPackage     [DbPackage]
            | RModule      [(DbPackageIdentifier, DbModule)]
            | RDeclaration [(DbPackageIdentifier, String, DbCompleteDecl)]
            | RConstructor [(DbPackageIdentifier, String, DbCompleteDecl, DbConstructor)]
            | RKeyword     String
            | RWarning     String -- ^ a warning

data Query = Query String

-- | status of hoogle operation
data HoogleStatus = Missing | OK | Error
        deriving (Show,Read,Eq,Ord,Enum,Bounded)