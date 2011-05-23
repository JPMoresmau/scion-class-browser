module Scion.PackageDB.Builder where

import Scion.PackageDB
import Scion.PackageDB.Parser
import Scion.PackageDB.Util
import System.Directory
import System.Exit
import System.FilePath
import System.Unix.Directory (withTemporaryDirectory, withWorkingDirectory)
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
                                         saveDatabase file db

-- | Downloads the information for the entire Hackage database
--   creating an in-memory database with it.
--   It needs a temporary directory to work on.
createHackageDatabase :: FilePath -> IO (Database, [(FilePath, ParseError)])
createHackageDatabase tmp =
  do let hoogleDbFile = tmp </> "hoogle.tar.gz"
         hoogleDbDir  = tmp </> "hoogle-db"
         baseDbFile   = tmp </> "base.txt"
         tmpDir       = tmp </> "tmp-db"
     executeCommand tmp "wget" [hoogleDbUrl, "-O", hoogleDbFile]
     createDirectoryIfMissing True hoogleDbDir
     executeCommand tmp "tar" ["-x", "-z", "-f", hoogleDbFile, "-C", hoogleDbDir]
     createDirectoryIfMissing True tmpDir
     (pkgs, errors) <- parseDirectory hoogleDbDir tmpDir
     executeCommand tmp "wget" [baseDbUrl, "-O", baseDbFile]
     basePkg <- parseHoogleFile baseDbFile
     case basePkg of
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
getCabalHoogle :: String -> String -> IO (Either ParseError (Doc Package))
getCabalHoogle pkg version = do withTemporaryDirectory "scionXXXXXX" (getCabalHoogleWithTmp pkg version)

getCabalHoogleWithTmp :: String -> String -> FilePath -> IO (Either ParseError (Doc Package))
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

