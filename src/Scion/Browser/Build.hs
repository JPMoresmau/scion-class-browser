module Scion.Browser.Build
( saveHackageDatabase
, createHackageDatabase
, updateDatabase
, createCabalDatabase
, getCabalHoogle
) where

import Control.Concurrent.ParallelIO.Local
import Control.DeepSeq
import Data.Either (rights)
import Data.List ((\\), nub)
import qualified Data.Map as M
import Data.Version (showVersion)
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (Package)
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

ghcDbUrl :: String
ghcDbUrl = "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt"

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
     logToStdout "Started downloading Hoogle database"
     Just hoogleDownloaded <- downloadFileLazy hoogleDbUrl
     logToStdout "Uncompressing Hoogle database"
     unTarGzip hoogleDownloaded hoogleDbDir
     logToStdout $ "Hoogle database is now in " ++ hoogleDbDir
     createDirectoryIfMissing True tmpDir
     (pkgs, errors) <- parseDirectory hoogleDbDir tmpDir
     -- Parse base package
     Just baseDownloaded <- downloadFileStrict baseDbUrl
     logToStdout "Base database successfully downloaded"
     -- Parse ghc package
     Just ghcDownloaded <- downloadFileStrict ghcDbUrl
     logToStdout "GHC database successfully downloaded"
     let (dbBase, errorsBase) = case parseHoogleString "base.txt" baseDownloaded of
                                  Right b -> (b:pkgs, errors)
                                  Left  e -> (pkgs, ("base.txt", e):errors)
     let (dbGhc, errorsGhc) = case parseHoogleString "ghc.txt" ghcDownloaded of
                                Right b -> (b:dbBase, errorsBase)
                                Left  e -> (dbBase, ("ghc.txt", e):errorsBase)
     return (pkgListToDb dbGhc, errorsGhc)

-- | Updates a database with changes in the installed package base.
updateDatabase :: Database -> [InstalledPackageInfo] -> IO Database
updateDatabase oldDb pkgInfo = do let dbList        = nub $ map fst $ M.toList oldDb
                                      installedList = nub $ removeSmallVersions $ map sourcePackageId pkgInfo
                                      toRemove      = dbList \\ installedList
                                      toAdd         = installedList \\ dbList
                                      filteredDb    = foldr (\pid db -> M.delete pid db) oldDb toRemove
                                  (addedDb, _) <- createCabalDatabase toAdd
                                  return $ M.union filteredDb addedDb

removeSmallVersions :: [PackageIdentifier] -> [PackageIdentifier]
removeSmallVersions pids = filter
  (not . (\(PackageIdentifier name version) -> 
             any (\(PackageIdentifier name' version') -> name' == name && version' > version) pids))
  pids

-- | Get the database from a set of Cabal packages.
createCabalDatabase :: [PackageIdentifier] -> IO (Database, [(String, ParseError)])
createCabalDatabase pkgs =
  withTemporaryDirectory "scionXXXXXX" $ \tmp ->
    do let toExecute = map (\pid -> do db <- getCabalHoogleWithTmp pid tmp
                                       return (pkgString pid, db))
                           pkgs
       eitherHooglePkgs <- withThreaded $ \pool -> parallelInterleavedE pool toExecute
       let hooglePkgs = rights eitherHooglePkgs
           (db, errors) = partitionPackages hooglePkgs
       return (db `deepseq` pkgListToDb db, errors)

-- | Get the database from a Cabal package.
getCabalHoogle :: PackageIdentifier -> IO (Either ParseError (Documented Package))
getCabalHoogle pid = do withTemporaryDirectory "scionXXXXXX" (getCabalHoogleWithTmp pid)

getCabalHoogleWithTmp :: PackageIdentifier -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogleWithTmp pid tmp = 
  do let pkgV = pkgString pid
         (PackageName pkg) = pkgName pid
     logToStdout $ "Parsing " ++ pkgV
     code <- executeCommand tmp "cabal" ["unpack", pkgV] True
     case code of
       ExitFailure _ -> return $ Left (newErrorMessage (Message "package not found")
                                                       (newPos pkgV 0 0))
       ExitSuccess ->
         do let pkgdir = tmp </> pkgV
            withWorkingDirectory pkgdir $
              do executeCommand pkgdir "cabal" ["configure"] True
                 executeCommand pkgdir "cabal" ["haddock", "--hoogle"] True
                 let hoogleFile = pkgdir </> "dist" </> "doc" </> "html" </> pkg </> (pkg ++ ".txt")
                 parseHoogleFile hoogleFile

pkgString :: PackageIdentifier -> String
pkgString (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version

