{-# LANGUAGE ScopedTypeVariables #-}

module Scion.PersistentBrowser.Build
( saveHackageDatabase
, createHackageDatabase
, updateDatabase
, createCabalDatabase
, getCabalHoogle
) where

import Control.Concurrent.ParallelIO.Local
import Control.Exception as E (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.List ((\\), nub)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Version (Version, showVersion, parseVersion)
import Database.Persist.Sqlite
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (Package)
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.FileUtil
import Scion.PersistentBrowser.Parser
import Scion.PersistentBrowser.ToDb
import Scion.PersistentBrowser.Types
import Scion.PersistentBrowser.Util
import System.Directory
import System.Exit
import System.FilePath
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)
import Text.ParserCombinators.ReadP


baseDbUrl :: String
baseDbUrl = "http://haskell.org/hoogle/base.txt"

ghcDbUrl :: String
ghcDbUrl = "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/ghc.txt"

hoogleDbUrl :: String
hoogleDbUrl = "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz"

-- | Gets the url of a package from Hackage
getPackageUrlHackage :: PackageIdentifier -> String
getPackageUrlHackage (PackageIdentifier (PackageName name) version) =
  "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ showVersion version ++ "/doc/html/" ++ name ++ ".txt"

-- | Gets the version of GHC used
getGhcInstalledVersion :: [PackageIdentifier] -> Version
getGhcInstalledVersion []     = error "No GHC found"
getGhcInstalledVersion ((PackageIdentifier (PackageName "ghc") version):_) = version
getGhcInstalledVersion (_:xs) = getGhcInstalledVersion xs

-- | Gets the url of a package from GHC libraries
getPackageUrlGhcLibs :: Version -> PackageIdentifier -> String
getPackageUrlGhcLibs ghcVersion (PackageIdentifier (PackageName name) version) =
  "http://www.haskell.org/ghc/docs/" ++ showVersion ghcVersion ++ "/html/libraries/" ++ name ++ "-" ++ showVersion version ++ "/" ++ name ++ ".txt"

-- | Downloads the information for the entire Hackage database
--   and saves it to the specified location.
saveHackageDatabase :: FilePath -> IO ()
saveHackageDatabase file = withTemporaryDirectory (saveHackageDatabaseWithTmp file)

saveHackageDatabaseWithTmp :: FilePath -> FilePath -> IO ()
saveHackageDatabaseWithTmp file tmp = do (db, _) <- createHackageDatabase tmp
                                         withSqliteConn (T.pack file) (runSqlConn (mapM_ savePackageToDb db))
                                         --mapM_ (\pkg -> withSqliteConn (T.pack file) (runSqlConn (savePackageToDb pkg))) db

-- | Downloads the information for the entire Hackage database
--   creating an in-memory database with it.
--   It needs a temporary directory to work on.
createHackageDatabase :: FilePath -> IO ([Documented Package], [(FilePath, ParseError)])
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
     --let (pkgs, errors) = ([], [])
     {-
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
     -}
     return (pkgs, errors)

-- | Updates a database with changes in the installed package base.
updateDatabase :: FilePath -> [InstalledPackageInfo] -> IO ()
updateDatabase file pkgInfo = withSqliteConn (T.pack file) $ runSqlConn $ updateDatabase' pkgInfo

updateDatabase' :: [InstalledPackageInfo] -> SqlPersist IO ()
updateDatabase' pkgInfo = 
  do dbPersistent <- selectList ([] :: [Filter DbPackage]) []
     let dbList        = map (fromDbToPackageIdentifier . entityVal) dbPersistent
         installedList = nub $ removeSmallVersions $ map sourcePackageId pkgInfo
         toRemove      = dbList \\ installedList
         toAdd         = installedList \\ dbList
     liftIO $ logToStdout $ "Removing " ++ show (map (\(PackageIdentifier (PackageName name) _) -> name) toRemove)
     mapM_ deletePackageByInfo toRemove
     liftIO $ logToStdout $ "Adding " ++ show (map (\(PackageIdentifier (PackageName name) _) -> name) toAdd)
     let ghcVersion = getGhcInstalledVersion installedList
     (addedDb, errors) <- liftIO $ createCabalDatabase' ghcVersion toAdd True
     mapM_ savePackageToDb addedDb
     liftIO $ logToStdout $ show errors

fromDbToPackageIdentifier :: DbPackage -> PackageIdentifier
fromDbToPackageIdentifier (DbPackage name version _) = PackageIdentifier (PackageName name)
                                                                         (getVersion version)
getVersion :: String -> Version
getVersion version = (fst . last . readP_to_S parseVersion) version

removeSmallVersions :: [PackageIdentifier] -> [PackageIdentifier]
removeSmallVersions pids = filter
  (not . (\(PackageIdentifier name version) -> 
             any (\(PackageIdentifier name' version') -> name' == name && version' > version) pids))
  pids

-- | Get the database from a set of Cabal packages.
createCabalDatabase :: Version -> [PackageIdentifier] -> IO ([Documented Package], [(String, ParseError)])
createCabalDatabase ghcVersion pkgs = createCabalDatabase' ghcVersion pkgs False

-- | Get the database from a set of Cabal packages.
--   If `ifFailCreateEmpty' is set, when a package gives a parse error,
--   it is converted into an empty package with a note.
createCabalDatabase' :: Version -> [PackageIdentifier] -> Bool -> IO ([Documented Package], [(String, ParseError)])
createCabalDatabase' ghcVersion pkgs ifFailCreateEmpty =
  withTemporaryDirectory $ \tmp ->
    do let toExecute = map (\pid -> do db <- getCabalHoogle ghcVersion pid ifFailCreateEmpty tmp
                                       return (pkgString pid, db))
                           pkgs
       eitherHooglePkgs <- withThreaded $ \pool -> parallelInterleavedE pool toExecute
       let hooglePkgs = rights eitherHooglePkgs
           (db, errors) = partitionPackages hooglePkgs
       return (db, errors)

-- | Get the database from a Cabal package.
getCabalHoogle :: Version -> PackageIdentifier -> Bool -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogle ghcVersion pid ifFailCreateEmpty tmp = 
  E.catch (do result <- getCabalHoogle' ghcVersion pid tmp
              case result of
                Left e                     -> return $ failure e
                Right (Package doc _ info) -> return $ Right (Package doc pid info))
          (\(e :: E.SomeException) -> return $ failure (newErrorMessage (Message "error parsing")
                                                                       (newPos "" 0 0)))
  where failure e = if ifFailCreateEmpty
                       then Right (Package NoDoc pid M.empty)
                       else Left e

-- | Get the database from a Cabal package.
getCabalHoogle' :: Version -> PackageIdentifier -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogle' ghcVersion pid tmp = do let downUrl1 = getPackageUrlHackage pid
                                        logToStdout $ "Download " ++ downUrl1
                                        tryDownload1 <- downloadHoogleFile downUrl1
                                        case tryDownload1 of
                                          Nothing   -> do let downUrl2 = getPackageUrlGhcLibs ghcVersion pid
                                                          logToStdout $ "Download " ++ downUrl2
                                                          tryDownload2 <- downloadHoogleFile downUrl2
                                                          case tryDownload2 of
                                                            Nothing   -> getCabalHoogleLocal pid tmp
                                                            Just cnts -> return $ parseHoogleString "<package>" cnts
                                          Just cnts -> return $ parseHoogleString "<package>" cnts

-- | Get the database from a locally installed Cabal package.
getCabalHoogleLocal :: PackageIdentifier -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogleLocal pid tmp = 
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

