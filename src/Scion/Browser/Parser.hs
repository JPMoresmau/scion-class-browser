{-# LANGUAGE ScopedTypeVariables #-}

module Scion.Browser.Parser
( parseHoogleString
, parseHoogleFile
, parseDirectory
) where

import Control.DeepSeq
import Control.Monad
import qualified Data.ByteString as BS
import Data.Binary
import Data.Monoid (mconcat)
import Scion.Browser
import Scion.Browser.Parser.Internal (hoogleParser)
import Scion.Browser.Util
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.IO
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)
import Text.Parsec.ByteString as BS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error (newErrorMessage, Message(..))
import Text.ParserCombinators.Parsec.Pos (newPos)

-- | Parses the contents of a string containing the 
--   Hoogle file contents.
parseHoogleString :: String -> BS.ByteString -> Either ParseError (Documented Package)
parseHoogleString name contents = case runP hoogleParser () name contents of
                                    Left e    -> Left e
                                    Right pkg -> pkg `deepseq` Right pkg

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
     fPackages <- mapM (\fname -> do putStrLn $ "parsing " ++ fname
                                     p <- parseHoogleFile fname
                                     return (fname, p) )
                       files
     let (pkgs, errors) = partitionPackages fPackages
     return $ pkgs `deepseq` (pkgs, errors)

