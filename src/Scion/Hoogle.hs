module Scion.Hoogle where

import Data.List (find)
import Distribution.InstalledPackageInfo
import Distribution.Package
import Scion.Packages
import System.FilePath

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
getDatabasesDir path = let (_:(hoogleV:(lib:rest))) = reverse $ splitDirectories path
                       in  (joinPath $ reverse (hoogleV:("share":rest))) </> "databases"

