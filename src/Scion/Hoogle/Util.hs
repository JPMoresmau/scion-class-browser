{-# LANGUAGE CPP #-}

module Scion.Hoogle.Util
( findHoogleBinPath
) where

import Data.List (find)
import Distribution.InstalledPackageInfo
import Distribution.Package
import Scion.Packages
import System.FilePath
import System.Directory (doesFileExist)

import System.Directory (getAppUserDataDirectory)

-- Functions for finding Hoogle in the system

findHoogleBinPath :: IO (Maybe String)
findHoogleBinPath = findPathsAndCheck placesToSearch
                    where placesToSearch = [ findHoogleBinInLibrary getHoogleBinPath1
                                           , findHoogleBinInLibrary getHoogleBinPath2
                                           , getHoogleBinPath3
                                           ]

findPathsAndCheck :: [IO (Maybe String)] -> IO (Maybe String)
findPathsAndCheck []     = return Nothing
findPathsAndCheck (f:fs) = do r <- findPathAndCheck f
                              case r of
                                Nothing -> findPathsAndCheck fs
                                _       -> return r

findPathAndCheck :: IO (Maybe String) -> IO (Maybe String)
findPathAndCheck f = do p <- f
                        case p of
                          Nothing   -> return Nothing
                          Just path -> do exists <- doesFileExist path
                                          if exists
                                             then return (Just path)
                                             else return Nothing

findHoogleBinInLibrary :: (String -> String) -> IO (Maybe String)
findHoogleBinInLibrary f = do minfo <- findHoogleInfo
                              case minfo of
                                Nothing   -> return Nothing
                                Just info -> let [libDir] = libraryDirs info
                                             in  return $ Just (f libDir)

findHoogleInfo :: IO (Maybe InstalledPackageInfo)
findHoogleInfo = do infos' <- getPkgInfos
                    let infos = removeSmallVersions $ concat $ map snd infos'
                    return $ find (\m -> (pkgName (sourcePackageId m)) == PackageName "hoogle") infos

removeSmallVersions :: [InstalledPackageInfo] -> [InstalledPackageInfo]
removeSmallVersions pids = filter
  (not . (\InstalledPackageInfo { sourcePackageId = (PackageIdentifier name version) } -> 
             any (\InstalledPackageInfo { sourcePackageId = (PackageIdentifier name' version') } ->
                     name' == name && version' > version)
                 pids))
  pids

getHoogleBinPath1 :: String -> String
getHoogleBinPath1 path = let (_:(_:(_:rest))) = reverse $ splitDirectories path
                         in  (joinPath $ reverse ("bin":rest)) </> "hoogle" <.> exeExtension

getHoogleBinPath2 :: String -> String
getHoogleBinPath2 path = let (_:(_:rest)) = reverse $ splitDirectories path
                         in  (joinPath $ reverse ("bin":rest)) </> "hoogle" <.> exeExtension

getHoogleBinPath3 :: IO (Maybe String)
getHoogleBinPath3 = do cabalDir <- getAppUserDataDirectory "cabal"
                       return $ Just (cabalDir </> "bin" </> "hoogle" <.> exeExtension)

exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif

-- This part is commented out because Cabal does not work
-- well linking with the Hoogle library.
-- Instead, `hoogle` is called directly and the results
-- are parsed and converted into database items.

{-
-- |Loads the Hoogle search database into memory.
--  If no database is found, an empty one is returned.
getHoogleDatabases :: IO H.Database
getHoogleDatabases = do path <- findHoogleDatabasesPath
                        case path of
                          Nothing -> return mempty
                          Just p  -> do files <- getDirectoryContents p
                                        hooFiles <- filterM (\f -> do exists <- doesFileExist f
                                                                      let isHoo = takeExtension f == ".hoo"
                                                                      return $ exists && isHoo)
                                                            files
                                        dbs <- mapM H.loadDatabase hooFiles
                                        return $ mconcat dbs

findHoogleDatabasesPath :: IO (Maybe String)
findHoogleDatabasesPath = do minfo <- findHoogleInfo
                             case minfo of
                               Nothing   -> return Nothing
                               Just info -> let [libDir] = libraryDirs info
                                            in  return $ Just (getDatabasesDir libDir)

getDatabasesDir :: String -> String
getDatabasesDir path = let (_:(hoogleV:(_:rest))) = reverse $ splitDirectories path
                       in  (joinPath $ reverse (hoogleV:("share":rest))) </> "databases"
-}

