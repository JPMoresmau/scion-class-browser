{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Server.Commands
import System.Console.Haskeline

import Database.Persist
import Database.Persist.Sqlite
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Build
import Scion.Packages

{-
main :: IO ()
main = do runStateT (runInputT defaultSettings loop) initialState
          return ()
-}

main :: IO ()
main = do withSqliteConn "local.persist" $ runSqlConn $ runMigration migrateAll
          pkgInfos' <- getPkgInfos
          let pkgInfos = take 20 $ concat $ map snd pkgInfos'
          updateDatabase "local.persist" pkgInfos
    

loop :: InputT BrowserM ()
loop = do maybeLine <- getInputLine ""
          case maybeLine of
            Nothing -> return () -- ctrl+D or EOF
            Just line -> do
              case Atto.parse json (BS.pack line) of
                Atto.Fail _ _ e   -> outputStrLn ("error in command: " ++ e) >> loop
                Atto.Done _ value -> case T.parse parseJSON value of
                                       Error e     -> outputStrLn ("error in command: " ++ e) >> loop
                                       Success cmd -> do (res, continue) <- lift $ executeCommand cmd
                                                         liftIO $ LBS.putStrLn $ encode res
                                                         if continue then loop else return ()

