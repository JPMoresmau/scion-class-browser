{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.Attoparsec.Types as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Server.PersistentCommands
import System.Console.Haskeline
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Version (showVersion)
import Paths_scion_browser

main :: IO ()
main = do
        args<-getArgs 
        case args of
                ("--version":_)->do
                        putStrLn ("scion-browser executable, version "++ (showVersion version))
                _-> do
                        runStateT (runInputT defaultSettings loop) initialState
                        return ()

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
                                                         let encoded    = LBS.append (encode res) "\n"
                                                             compressed = Zlib.compressWith Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestSpeed } encoded
                                                         liftIO $ LBS.putStr compressed
                                                         liftIO $ hFlush stdout
                                                         if continue then loop else return ()

