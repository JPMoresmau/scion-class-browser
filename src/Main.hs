module Main where

import Scion.Browser
import Scion.Browser.Parser.Documentable
import Scion.Browser.Parser.Internal
import Language.Haskell.Exts.Parser
-- import Scion.Browser.Builder
-- import Scion.Browser.Installed
-- import Scion.Browser.Parser
-- import Text.Show.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runP)

import Text.Parsec.Combinator
import Text.Parsec.Prim

main :: IO ()
main = do -- (db, errors) <- createHackageDatabase "/home/serras/gsoc/haskell-workspace/example"
          -- (db, errors) <- parseDirectory "/home/serras/gsoc/haskell-workspace/example/hoogle-db"
          --                                "/home/serras/gsoc/haskell-workspace/example/tmp-db"
          -- installed <- getInstalledPackages
          -- db <- updateDatabase [] installed
          -- mapM_ (\(Doc _ (Package n v _)) -> putStrLn $ n ++ "-" ++ v) db
          -- let encoded = encode db
          -- BS.writeFile "/home/serras/gsoc/haskell-workspace/example.db" encoded
          case runP (data_ NoDoc) () "zas" (BS8.pack "data Show a => Ejemplo a :: * -> *\nZas :: t -> Ejemplo a\nBol :: v -> Ejemplo b") of
            Left e -> putStrLn $ show e
            Right t -> putStrLn $ show t
          case parseType "Show a b" of
            ParseOk t -> putStrLn $ show (lineariseType $ document NoDoc t)
            ParseFailed _ s -> putStrLn $ show s
          putStrLn "Finished"
