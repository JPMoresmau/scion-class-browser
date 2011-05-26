{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scion.Browser
import Scion.Browser.Parser.Documentable
import Scion.Browser.Parser.Internal
import Scion.Browser.Parser
import Language.Haskell.Exts.Parser
-- import Scion.Browser.Builder
-- import Scion.Browser.Installed
-- import Scion.Browser.Parser
-- import Text.Show.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Serialize
import Distribution.Package hiding (Package)

import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)

import Text.Parsec.Combinator
import Text.Parsec.Prim

main :: IO ()
main = do --(db, errors) <- createHackageDatabase "/home/serras/gsoc/haskell-workspace/example"
          --tt <- parseHoogleFile "/home/serras/gsoc/haskell-workspace/example/hoogle-db/mtl/2.0.1.0/doc/html/mtl.txt"
          --case tt of
          --  Right pkg -> putStrLn $ show pkg
          --  Left e -> putStrLn $ show e
          {- (db, errors) <- parseDirectory "/home/serras/gsoc/haskell-workspace/example/hoogle-db"
                                         "/home/serras/gsoc/haskell-workspace/example/tmp-db"
          putStrLn $ show (length errors)
          putStrLn $ show errors -}
          -- installed <- getInstalledPackages
          -- db <- updateDatabase [] installed
          -- mapM_ (\(Doc _ (Package n v _)) -> putStrLn $ n ++ "-" ++ v) db
          {- let encoded = encode db
          BS.writeFile "/home/serras/gsoc/haskell-workspace/example.db" encoded
          putStrLn "Finished writing" -}
          {-case runP (newtype_ NoDoc) () "zas" (BS8.pack "newtype ErrorT e m :: (* -> *) a :: * -> (* -> *) -> * -> *") of
            Left e -> putStrLn $ show e
            Right t -> putStrLn $ show t-}
          {- case parseType "Show a b" of
            ParseOk t -> putStrLn $ show (lineariseType $ document NoDoc t)
            ParseFailed _ s -> putStrLn $ show s -}
          
          g <- BS.readFile "/home/serras/gsoc/haskell-workspace/example.db"
          let Right (t :: Database) = decode g
          mapM_ putStrLn (map (\pkg -> let (PackageName n) = packageName pkg in n) t)
          getChar
          
          putStrLn "Finished"
