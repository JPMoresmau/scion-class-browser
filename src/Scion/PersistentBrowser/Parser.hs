{-# LANGUAGE ScopedTypeVariables #-}

module Scion.PersistentBrowser.Parser
( parseHoogleString
, parseHoogleFile
, parseDirectory
) where

import Control.Concurrent.ParallelIO.Local
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Either (rights)
import Scion.PersistentBrowser.Types
import Scion.PersistentBrowser.Parser.Internal (hoogleParser)
import Scion.PersistentBrowser.FileUtil
import Scion.PersistentBrowser.Util
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.IO
import Text.Parsec.Error (Message(..), newErrorMessage)
import Text.Parsec.Prim (runP)
-- import Text.Parsec.ByteString as BS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos (newPos)

-- | Parses the contents of a string containing the 
--   Hoogle file contents.
parseHoogleString :: String -> BS.ByteString -> Either ParseError (Documented Package)
parseHoogleString name contents = runP hoogleParser () name (BSU.toString contents)

-- | Parses a file in Hoogle documentation format, returning
--   the documentation of the entire package, or the corresponding
--   error during the parsing.
parseHoogleFile :: FilePath -> IO (Either ParseError (Documented Package))
parseHoogleFile fname = (withFile fname ReadMode $
                           \hnd -> do c <- BS.hGetContents hnd
                                      return $ parseHoogleString fname c
                        )
                        `catch`
                        (\_ -> return $ Left (newErrorMessage (Message "error reading file")
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
parseDirectory :: FilePath -> FilePath -> IO ([Documented Package], [(FilePath, ParseError)])
parseDirectory dir tmpdir = 
  do contents' <- getDirectoryContents dir
     let contents = map (\d -> dir </> d) (filterDots contents')
     dirs <- filterM doesDirectoryExist contents
     vDirs <- mapM getVersionDirectory dirs
     let innerDirs = map (\d -> d </> "doc" </> "html") (concat vDirs)
     -- Parse directories recursively
     let toExecute = map (\innerDir -> parseDirectoryFiles innerDir tmpdir) innerDirs
     eitherDPackages <- withThreaded $ \pool -> parallelInterleavedE pool toExecute
     let dPackages = rights eitherDPackages
         dbs       = concat $ map fst dPackages
         errors    = concat $ map snd dPackages
     return (dbs, errors)

getVersionDirectory :: FilePath -> IO [FilePath]
getVersionDirectory dir = do contents' <- getDirectoryContents dir
                             let contents = map (\d -> dir </> d) (filterDots contents')
                             filterM doesDirectoryExist contents

parseDirectoryFiles :: FilePath -> FilePath -> IO ([Documented Package], [(FilePath, ParseError)])
parseDirectoryFiles dir tmpdir =
  do contents' <- getDirectoryContents dir
     let contents = map (\d -> dir </> d) (filterDots contents')
     files <- filterM doesFileExist contents
     fPackages <- mapM (\fname -> do hPutChar stderr '.' >> hFlush stderr
                                     p <- parseHoogleFile fname
                                     return (fname, p) )
                       files
     return $ partitionPackages fPackages

