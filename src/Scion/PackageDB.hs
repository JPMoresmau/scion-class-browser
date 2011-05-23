{-# LANGUAGE TemplateHaskell #-}

module Scion.PackageDB where

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.DeriveTH

-- | A documented entity
data Doc a = Doc { doc  :: String
                 , item :: a
                 }

-- | A data constructor
--    ConstructorName :: ConstructorType
data DataConstructor = DataConstructor { conName :: String
                                       , conType :: String
                                       }

-- | Items that may appear as leaves
--   in the tree of packages
data Item = 
            -- | Declaration of a abstract data type
            --    data Name [params] [:: kind]
            --   followed by the set of constructors
            Data     { dataName         :: String
                     , dataParams       :: [String]
                     , dataConstructors :: [Doc DataConstructor]
                     , dataKind         :: Maybe String
                     , dataPrereq       :: [String]
                     }
            -- | Newtype haskell declaration
            --    newtype Name [params] [:: kind]
            --   followed by the type constructor
          | Newtype  { typeName         :: String
                     , typeParams       :: [String]
                     , dataConstructors :: [Doc DataConstructor]
                     , dataKind         :: Maybe String
                     , dataPrereq       :: [String]
                     }
            -- | Declaration of a type class
            --    class [prereq =>] Name [params]
            --                      [ | fun-deps ]
            --                      [ where { type-fam } ]
          | Class    { className    :: String
                     , classParams  :: [String]
                     , classPrereq  :: [String]
                     , classKind    :: Maybe String
                     , classFunDeps :: Maybe String
                     , classTypeFam :: Maybe String
                     }
            -- | Declaration of an instance
            --    instance Declaration
          | Instance { instDecl :: String
                     }
            -- | Declaration of a type synonym
            --    type Name [params] = Declaration
          | Type     { typeName   :: String
                     , typeParams :: [String]
                     , typeDecl   :: String
                     }
            -- | Function declaration
            --    name :: Type
          | Function { fnName :: String
                     , fnType :: String
                     }

-- | Represents a module in the package.
--   Modules are arranged hierarchically.
data Module = Module { modName    :: String
                     , modItems   :: [Doc Item]
                     , modModules :: [Doc Module]
                     }

-- | Represents an entire package, consisting
--   of its name, version and list of modules.
data Package = Package { pkgName    :: String
                       , pkgVersion :: String
                       , pkgModules :: [Doc Module]
                       }

-- | A database is a set of packages
type Database = [Doc Package]

-- | Loads a database from a file
loadDatabase :: FilePath -> IO (Either String Database)
loadDatabase file = do enc <- BS.readFile file
                       return $ decode enc 

-- | Saves a database into a file
saveDatabase :: FilePath -> Database -> IO ()
saveDatabase file db = BS.writeFile file (encode db)


$( derive makeShow ''Doc )
$( derive makeShow ''DataConstructor )
$( derive makeShow ''Item )
$( derive makeShow ''Module )
$( derive makeShow ''Package )

$( derive makeSerialize ''Doc )
$( derive makeSerialize ''DataConstructor )
$( derive makeSerialize ''Item )
$( derive makeSerialize ''Module )
$( derive makeSerialize ''Package )

-- Needed to run DeepSeq
$( derive makeNFData ''Doc )
$( derive makeNFData ''DataConstructor )
$( derive makeNFData ''Item )
$( derive makeNFData ''Module )
$( derive makeNFData ''Package )

