module Scion.Browser.Query
( allPackageIds
, allPackages
, packagesByName
, getPackage
, getSingletonDatabase
, getModules
, getSubmodules
, getDeclsInModule
) where

import Data.List (isPrefixOf, stripPrefix)
import Data.List.Utils (addToAL)
import qualified Data.Map as M
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser

-- |Get the identifiers of all packages in the database.
allPackageIds :: Database -> [PackageIdentifier]
allPackageIds = M.keys

-- |Get information of all packages in the database.
allPackages :: Database -> [Documented Package]
allPackages = M.elems

-- |Get information of all versions of the package with that name.
packagesByName :: String -> Database -> [Documented Package]
packagesByName name db = M.foldlWithKey (\lst k v -> let PackageName pkgN = pkgName k
                                                     in if pkgN == name then v:lst else lst)
                                        [] db

-- |Get information about a package in the database.
getPackage :: PackageIdentifier -> Database -> Maybe (Documented Package)
getPackage = M.lookup

-- |Builds a database with only the specified package.
getSingletonDatabase :: PackageIdentifier -> Database -> Maybe Database
getSingletonDatabase pid db = case getPackage pid db of
                                Nothing  -> Nothing
                                Just pkg -> Just $ singletonDatabase pkg

type ModuleWithPackage = (PackageIdentifier, Documented Module)

-- |Get all the modules hierarchically inside the specified one.
--  This function only goes one level deep, for the entire list
--  use getSubmodules.
--  For getting the upper modules, use "" as initial name.
getModules :: String -> Database -> [(String, [ModuleWithPackage])]
getModules = getSubmodules' getModulesFromPackage

-- |Get all the modules hierarchically inside the specified one.
--  For getting the entire list of modules modules, use "" as initial name.
getSubmodules :: String -> Database -> [(String, [ModuleWithPackage])]
getSubmodules = getSubmodules' getSubmodulesFromPackage

getSubmodules' :: (String -> Documented Package -> [Documented Module]) -> String -> Database -> [(String, [ModuleWithPackage])]
getSubmodules' f mname db = M.foldrWithKey (\pid pkg current -> mergeModules pid (f mname pkg) current) [] db

mergeModules :: PackageIdentifier -> [Documented Module] -> [(String, [ModuleWithPackage])] -> [(String, [ModuleWithPackage])]
mergeModules pid mods initial = foldr (\md current -> mergeModule pid md current) initial mods

mergeModule :: PackageIdentifier -> Documented Module -> [(String, [ModuleWithPackage])] -> [(String, [ModuleWithPackage])]
mergeModule pid md current = let mname = getName md
                              in case lookup mname current of
                                   Nothing   -> (mname, [(pid, md)]) : current
                                   Just mods -> addToAL current mname ((pid, md):mods)

getModulesFromPackage :: String -> Documented Package -> [Documented Module]
getModulesFromPackage ""      (Package _ _ modMap) =
  M.foldlWithKey (\lst k v -> if '.' `elem` k then lst else v:lst) [] modMap
getModulesFromPackage initial (Package _ _ modMap) = 
  M.foldlWithKey (\lst k v -> if include k then v:lst else lst) [] modMap
  where minitial = initial ++ "."
        include k = case stripPrefix minitial k of
                      Nothing -> False
                      Just mn -> not ('.' `elem` mn)

getSubmodulesFromPackage :: String -> Documented Package -> [Documented Module]
getSubmodulesFromPackage ""      (Package _ _ modMap) = M.elems modMap
getSubmodulesFromPackage initial (Package _ _ modMap) = 
  let minitial = initial ++ "."
  in  M.foldlWithKey (\lst k v -> if minitial `isPrefixOf` k then v:lst else lst) [] modMap

-- |Gets the declarations inside some module,
--  along with information about which package it lives.
getDeclsInModule :: String -> Database -> [(PackageIdentifier, [Documented Decl])]
getDeclsInModule modName db = M.foldrWithKey (\k v lst -> (k, getDeclsInModuleFromPackage modName v):lst) [] db

getDeclsInModuleFromPackage :: String -> Documented Package -> [Documented Decl]
getDeclsInModuleFromPackage modName (Package _ _ modMap) =
  case M.lookup modName modMap of
    Just (Module _ _ _ _ decls) -> decls
    _                           -> []

