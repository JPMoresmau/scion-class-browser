{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Scion.Browser.Parser
( parseHoogleString
, parseHoogleFile
, parseDirectory
) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Monoid (mconcat)
import Scion.Browser
import Scion.Browser.Parser.Internal (hoogleParser)
import Scion.Browser.Util
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.IO
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Prim (runP)
-- import Text.Parsec.ByteString as BS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)

-- | Parses the contents of a string containing the 
--   Hoogle file contents.
parseHoogleString :: String -> BS.ByteString -> Either ParseError (Documented Package)
parseHoogleString name contents = runP hoogleParser () name contents

-- | Parses a file in Hoogle documentation format, returning
--   the documentation of the entire package, or the corresponding
--   error during the parsing.
parseHoogleFile :: FilePath -> IO (Either ParseError (Documented Package))
parseHoogleFile fname = (withFile fname ReadMode $
                           \hnd -> do c <- BS.hGetContents hnd
                                      return $ parseHoogleString fname c
                        )
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
     let dbs    = mconcat $ map fst dPackages
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
     fPackages <- mapM (\fname -> do putChar '.'
                                     hFlush stdout
                                     p <- parseHoogleFile fname
                                     -- return (fname, p)
                                     case p of
                                       Left _ -> return (fname, p)
                                       Right pkg -> do let tmpFile = tmpdir </> takeFileName fname
                                                       withFile tmpFile WriteMode $
                                                         \hnd -> BS.hPut hnd (encode pkg)
                                                       s <- withFile tmpFile ReadMode $
                                                              \hnd -> do s <- BS.hGetContents hnd
                                                                         return s
                                                       let Right (pkg' :: Documented Package) = decode s
                                                       return (fname, Right pkg') )
                       files
     return $ partitionPackages fPackages

