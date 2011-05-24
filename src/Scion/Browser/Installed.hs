module Scion.Browser.Installed
( getInstalledPackages
, updateDatabase
) where

import Data.List
import Distribution.InstalledPackageInfo
import Distribution.Package (PackageName(..), PackageIdentifier (..), pkgName)
import Distribution.Version
import GHC.Paths (ghc_pkg)
import Scion.Browser
import Scion.Browser.Builder (createCabalDatabase)
import Scion.Browser.Util
import System.Directory
import System.FilePath
import System.IO.Error
import System.Process (readProcess)

-- | Get a information about the installed packages
--   available in both the general and the
--   user-specific library directories.
getInstalledPackages :: IO [InstalledPackageInfo]
getInstalledPackages = do
    dirs <- getConfDirs
    infos <- mapM getInfo dirs
    return $ concat infos

-- | Get package information from a single directory.
getInfo :: FilePath -> IO [InstalledPackageInfo]
getInfo dir = do files <- getDirectoryContents dir
                 let confs = [ dir </> file
                             | file <- files
                             , isSuffixOf ".conf" file]
                 mapMerr (\file -> do cnts <- readFile file
                                      -- Force reading of the file
                                      let contents = cnts `seq` cnts
                                      let parsing = parseInstalledPackageInfo contents
                                      case parsing of
                                        ParseFailed _ -> error "parse failed"
                                        ParseOk _ r   -> return r)
                         confs

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

-- taken from ghc-pkg-autofix package
-- | Get the directories used for finding packages in GHC
getConfDirs :: IO [FilePath]
getConfDirs = do
  src <- readProcess ghc_pkg ["list"] ""
  return $ map init $ filter (\x -> not (null x) && head x == '/') $ lines src

