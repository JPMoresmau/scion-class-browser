module Scion.Hoogle
( getHoogleDatabases
) where

import Control.Monad
import Data.List (find)
import Data.Monoid
import Distribution.InstalledPackageInfo
import Distribution.Package
import qualified Hoogle as H
import Scion.Packages
import System.Directory
import System.FilePath

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

getDatabasesDir :: String -> String
getDatabasesDir path = let (_:(hoogleV:(_:rest))) = reverse $ splitDirectories path
                       in  (joinPath $ reverse (hoogleV:("share":rest))) </> "databases"

