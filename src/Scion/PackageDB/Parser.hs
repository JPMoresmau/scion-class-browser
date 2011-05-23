module Scion.PackageDB.Parser
( parseHoogleString
, parseHoogleFile
, parseDirectory
) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Serialize
import Scion.PackageDB
import Scion.PackageDB.Parser.Stage1
import Scion.PackageDB.Parser.Stage2
import Scion.PackageDB.Util
import System.Directory
import System.FilePath ((</>), takeFileName)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)
import Text.Parsec.ByteString as BS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)

-- | Parses the contents of a string containing the 
--   Hoogle file contents.
parseHoogleString :: String -> ByteString -> Either ParseError (Doc Package)
parseHoogleString name contents = case runP hoogleParser () name contents of
                                    Left err     -> Left err
                                    Right parsed -> convertHoogleToPackage name parsed

-- | Parses a file in Hoogle documentation format, returning
--   the documentation of the entire package, or the corresponding
--   error during the parsing.
parseHoogleFile :: FilePath -> IO (Either ParseError (Doc Package))
parseHoogleFile fname = do parsed <- BS.parseFromFile hoogleParser fname
                           case parsed of
                             Left error -> return $ Left error
                             Right p    -> return $ convertHoogleToPackage fname p
                        `catch`
                        (\e -> return $ Left (newErrorMessage (Message "error reading file")
                                                              (newPos fname 0 0)))

-- | Parses a entire directory of Hoogle documentation files
--   which must be following the format of the Hackage
--   Hoogle library, specifically:
--   
--   <root>
--     / package-name
--       / version
--         /doc/html/package-name.txt
-- 
parseDirectory :: FilePath -> FilePath -> IO (Database, [(FilePath, ParseError)])
parseDirectory dir tmpdir = 
  do contents' <- getDirectoryContents dir
     let contents = map (\d -> dir </> d) (filterDots contents')
     dirs <- filterM doesDirectoryExist contents
     vDirs <- mapM getVersionDirectory dirs
     let innerDirs = map (\d -> d </> "doc" </> "html") (concat vDirs)
     -- Parse directories recursively
     dPackages <- mapM (\dir -> parseDirectoryFiles dir tmpdir) innerDirs -- IO [...]
     let dbs    = concat $ map fst dPackages
         errors = concat $ map snd dPackages
     return (dbs, errors)

getVersionDirectory :: FilePath -> IO [FilePath]
getVersionDirectory dir = do contents' <- getDirectoryContents dir
                             let contents = map (\d -> dir </> d) (filterDots contents')
                             filterM doesDirectoryExist contents

parseDirectoryFiles :: FilePath -> FilePath -> IO (Database, [(FilePath, ParseError)])
parseDirectoryFiles dir tmpdir =
  do contents' <- getDirectoryContents dir
     let contents = map (\d -> dir </> d) (filterDots contents')
     files <- filterM doesFileExist contents
     fPackages <- mapM (\fname -> do putStrLn $ "parsing " ++ fname
                                     p <- parseHoogleFile fname
                                     case p of
                                       Left _ -> return (fname, p)
                                       Right pkg -> do let tmpFile = tmpdir </> takeFileName fname
                                                           tmpEncoded = encode pkg
                                                       -- encode and decode to file to save space
                                                       -- (it forces evaluation)
                                                       Data.ByteString.writeFile tmpFile tmpEncoded
                                                       tmpDecoded <- Data.ByteString.readFile tmpFile
                                                       let Right pkg' = decode tmpDecoded
                                                       return (fname, Right pkg'))
                       files
     return $ partitionPackages fPackages

