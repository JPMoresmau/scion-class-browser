module Scion.Browser.Builder where

import Data.List (intercalate, (\\), nub)
import qualified Data.Map as M
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (Package)
import Distribution.Version
import Scion.Browser
import Scion.Browser.Parser
import Scion.Browser.FileUtil
import Scion.Browser.Util
import System.Directory
import System.Exit
import System.FilePath
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)

baseDbUrl :: String
baseDbUrl = "http://haskell.org/hoogle/base.txt"

hoogleDbUrl :: String
hoogleDbUrl = "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz"

-- | Downloads the information for the entire Hackage database
--   and saves it to the specified location.
saveHackageDatabase :: FilePath -> IO ()
saveHackageDatabase file = withTemporaryDirectory "scionXXXXXX" (saveHackageDatabaseWithTmp file)

saveHackageDatabaseWithTmp :: FilePath -> FilePath -> IO ()
saveHackageDatabaseWithTmp file tmp = do (db, _) <- createHackageDatabase tmp
                                         saveDatabase file db

-- | Downloads the information for the entire Hackage database
--   creating an in-memory database with it.
--   It needs a temporary directory to work on.
createHackageDatabase :: FilePath -> IO (Database, [(FilePath, ParseError)])
createHackageDatabase tmp =
  do let hoogleDbDir  = tmp </> "hoogle-db"
         tmpDir       = tmp </> "tmp-db"
     -- Parse Hoogle database
     createDirectoryIfMissing True hoogleDbDir
     putStrLn "Started downloading Hoogle database"
     Just hoogleDownloaded <- downloadFileLazy hoogleDbUrl
     putStrLn "Uncompressing Hoogle database"
     unTarGzip hoogleDownloaded hoogleDbDir
     putStrLn $ "Hoogle database is now in " ++ hoogleDbDir
     createDirectoryIfMissing True tmpDir
     (pkgs, errors) <- parseDirectory hoogleDbDir tmpDir
     -- Parse base package
     Just baseDownloaded <- downloadFileStrict baseDbUrl
     putStrLn "Base database successfully downloaded"
     case parseHoogleString "base.txt" baseDownloaded of
       Right b -> return (pkgListToDb (b:pkgs), errors)
       Left  e -> return (pkgListToDb pkgs, ("base.txt", e):errors)

-- | Updates a database with changes in the installed package base.
updateDatabase :: Database -> [InstalledPackageInfo] -> IO Database
updateDatabase oldDb pkgInfo = do let dbList        = nub $ map fst $ M.toList oldDb
                                      installedList = nub $ map sourcePackageId pkgInfo
                                      toRemove      = dbList \\ installedList
                                      toAdd         = installedList \\ dbList
                                      filteredDb    = foldr (\pid db -> M.delete pid db) oldDb toRemove
                                  (addedDb, _) <- createCabalDatabase toAdd
                                  return $ M.union filteredDb addedDb

-- | Get the database from a set of Cabal packages.
createCabalDatabase :: [PackageIdentifier] -> IO (Database, [(String, ParseError)])
createCabalDatabase pkgs =
  do hooglePkgs <- mapM (\pid -> do db <- getCabalHoogle pid
                                    return (pkgString pid, db))
                        pkgs
     let (db, errors) = partitionPackages hooglePkgs
     return (pkgListToDb db, errors)

-- | Get the database from a Cabal package.
getCabalHoogle :: PackageIdentifier -> IO (Either ParseError (Documented Package))
getCabalHoogle pid = do withTemporaryDirectory "scionXXXXXX" (getCabalHoogleWithTmp pid)

getCabalHoogleWithTmp :: PackageIdentifier -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogleWithTmp pid tmp = 
  do let pkgV = pkgString pid
         (PackageName pkg) = pkgName pid
     code <- executeCommand tmp "cabal" ["unpack", pkg]
     case code of
       ExitFailure _ -> return $ Left (newErrorMessage (Message "package not found")
                                                       (newPos pkgV 0 0))
       ExitSuccess ->
         do let pkgdir = tmp </> pkgV
            withWorkingDirectory pkgdir $
              do executeCommand pkgdir "cabal" ["configure"]
                 executeCommand pkgdir "cabal" ["haddock", "--hoogle"]
                 let hoogleFile = pkgdir </> "dist" </> "doc" </> "html" </> pkg </> (pkg ++ ".txt")
                 parseHoogleFile hoogleFile

pkgString :: PackageIdentifier -> String
pkgString (PackageIdentifier (PackageName name) (Version branch _)) = name ++ "-" ++ (intercalate "." $ map show branch)

