{-# LANGUAGE TemplateHaskell #-}

module Scion.Browser
( Package (..)
, Database (..)
, MDatabase (..)
, module Scion.Browser.HSEInstances
, saveDatabase
, loadDatabase
, saveMDatabase
, loadMDatabase
, dbToMDb
) where

import Control.DeepSeq
import Data.Serialize
import Data.DeriveTH
import qualified Data.ByteString as BS
import qualified Data.Map as M
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
type Database = [Documented Package]

type MDatabase = M.Map PackageIdentifier (Documented Package)

saveDatabase :: FilePath -> Database -> IO ()
saveDatabase fpath db  = withFile fpath WriteMode $
                           \hnd -> BS.hPut hnd (encode db)

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase fpath = withFile fpath ReadMode $
                       \hnd -> do s <- BS.hGetContents hnd
                                  return $ case decode s of
                                             Left _  -> Nothing
                                             Right p -> p `deepseq` Just p

dbToMDb :: Database -> MDatabase
dbToMDb db = M.fromList (map (\pkg -> (packageId pkg, pkg)) db)

saveMDatabase :: FilePath -> MDatabase -> IO ()
saveMDatabase fpath db  = withFile fpath WriteMode $
                            \hnd -> BS.hPut hnd (encode db)

loadMDatabase :: FilePath -> IO (Maybe MDatabase)
loadMDatabase fpath = withFile fpath ReadMode $
                        \hnd -> do s <- BS.hGetContents hnd
                                   return $ case decode s of
                                              Left _  -> Nothing
                                              Right p -> p `deepseq` Just p


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

