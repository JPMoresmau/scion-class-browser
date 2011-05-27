module Scion.Browser.Installed
( getInstalledPackages
, updateDatabase
) where

import Data.List
import Distribution.InstalledPackageInfo
import Distribution.Package (PackageName(..), PackageIdentifier (..), pkgName)
import Distribution.Version
import Scion.Browser
import Scion.Browser.Builder (createCabalDatabase)
import Scion.Browser.Util
import System.Directory
import System.FilePath
import System.IO.Error

-- | Updates a database with changes in the installed package base.
updateDatabase :: Database -> [InstalledPackageInfo] -> IO Database
updateDatabase oldDb pkgInfo = do let dbList        = pkgListFromDatabase oldDb
                                      installedList = pkgListFromInstalledPkgs pkgInfo
                                      toRemove      = dbList \\ installedList
                                      toAdd         = installedList \\ dbList
                                      filteredDb    = filter (\(Doc _ (Package n v _)) -> not $ (n,v) `elem` toRemove) oldDb
                                  (addedDb, _) <- createCabalDatabase toAdd
                                  return $ filteredDb ++ addedDb
                                  

pkgListFromDatabase :: Database -> [(String, String)]
pkgListFromDatabase db = nub $ map (\(Doc _ (Package n v _)) -> (n, v)) db

pkgListFromInstalledPkgs :: [InstalledPackageInfo] -> [(String, String)]
pkgListFromInstalledPkgs infos = nub $ map (\(InstalledPackageInfo
                                                { sourcePackageId = PackageIdentifier
                                                                     (PackageName n) v }) -> (n, showVersion v))
                                           infos

showVersion :: Version -> String
showVersion (Version b []) = intercalate "." $ map show b
showVersion (Version b t)  = (intercalate "." $ map show b) ++ "-" ++ (intercalate "." t)

