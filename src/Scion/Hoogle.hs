module Scion.Hoogle where

import Data.List (find)
import Distribution.InstalledPackageInfo
import Distribution.Package
import Scion.Packages

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

