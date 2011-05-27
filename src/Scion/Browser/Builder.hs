module Scion.Browser.Builder where

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

baseDbUrl = "http://haskell.org/hoogle/base.txt"
hoogleDbUrl = "http://hackage.haskell.org/packages/archive/00-hoogle.tar.gz"

-- | Downloads the information for the entire Hackage database
--   and saves it to the specified location.
saveHackageDatabase :: FilePath -> IO ()
saveHackageDatabase file = withTemporaryDirectory "scionXXXXXX" (saveHackageDatabaseWithTmp file)

saveHackageDatabaseWithTmp :: FilePath -> FilePath -> IO ()
saveHackageDatabaseWithTmp file tmp = do (db, _) <- createHackageDatabase tmp
                                         saveMDatabase file (dbToMDb db)

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
       Right b -> return (b:pkgs, errors)
       Left  e -> return (pkgs, ("base.txt", e):errors)

-- | Get the database from a set of Cabal packages.
createCabalDatabase :: [(String, String)] -> IO (Database, [(String, ParseError)])
createCabalDatabase pkgs =
  do hooglePkgs <- mapM (\(n,v) -> do db <- getCabalHoogle n v
                                      return (n ++ "-" ++ v, db))
                        pkgs
     return $ partitionPackages hooglePkgs

-- | Get the database from a Cabal package.
getCabalHoogle :: String -> String -> IO (Either ParseError (Documented Package))
getCabalHoogle pkg version = do withTemporaryDirectory "scionXXXXXX" (getCabalHoogleWithTmp pkg version)

getCabalHoogleWithTmp :: String -> String -> FilePath -> IO (Either ParseError (Documented Package))
getCabalHoogleWithTmp pkg version tmp = 
  do let pkgV = pkg ++ "-" ++ version
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

