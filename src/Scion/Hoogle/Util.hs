module Scion.Hoogle.Util
( findHoogleBinPath
) where

import Data.List (find)
import Distribution.InstalledPackageInfo
import Distribution.Package
import Scion.Packages
import System.FilePath

-- Functions for finding Hoogle in the system

findHoogleBinPath :: IO (Maybe String)
findHoogleBinPath = do minfo <- findHoogleInfo
                       case minfo of
                         Nothing   -> return Nothing
                         Just info -> let [libDir] = libraryDirs info
                                      in  return $ Just (getHoogleBinPath libDir)

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

getHoogleBinPath :: String -> String
getHoogleBinPath path = let (_:(_:(_:rest))) = reverse $ splitDirectories path
                        in  (joinPath $ reverse ("bin":rest)) </> "hoogle"

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

