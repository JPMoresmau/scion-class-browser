module Main where

import Data.Serialize
import Scion.Browser
import Scion.Browser.Builder
import Scion.Browser.Installed
import Scion.Browser.Parser
import Text.Show.Pretty
import qualified Data.ByteString as BS

main :: IO ()
main = do -- (db, errors) <- createHackageDatabase "/home/serras/gsoc/haskell-workspace/example"
          -- (db, errors) <- parseDirectory "/home/serras/gsoc/haskell-workspace/example/hoogle-db"
          --                                "/home/serras/gsoc/haskell-workspace/example/tmp-db"
          installed <- getInstalledPackages
          db <- updateDatabase [] installed
          mapM_ (\(Doc _ (Package n v _)) -> putStrLn $ n ++ "-" ++ v) db
          -- let encoded = encode db
          -- BS.writeFile "/home/serras/gsoc/haskell-workspace/example.db" encoded
          putStrLn "Finished"
