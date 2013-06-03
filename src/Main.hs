{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.UTF8  as BSU(fromString)
import Server.PersistentCommands
import System.Console.Haskeline
import System.IO (hFlush, stdout, stderr)
import System.Environment (getArgs)
import Data.Version (showVersion)
import Paths_scion_browser
import Scion.PersistentBrowser.Util (logToStdout)

import GHC.IO.Handle (hDuplicate,hDuplicateTo)


main :: IO ()
main = do args <- getArgs 
          case args of
            ("--version":_) -> putStrLn ("scion-browser executable, version " ++ showVersion version)
            ("--clear":_)     -> run False
            _               -> run True
            
run :: Bool -> IO()       
run doCompression=do
        runStateT (runInputT defaultSettings (loop doCompression)) initialState
        return ()

loop :: Bool -> InputT BrowserM ()
loop doCompression = do 
          maybeLine <- getInputLine ""
          case maybeLine of
            Nothing -> return () -- ctrl+D or EOF
            Just line -> 
              case Atto.parseOnly json (BSU.fromString line) of
                Left e   -> liftIO (logToStdout ("error in command: " ++ e)) >> loop doCompression
                Right value -> case T.parse parseJSON value of
                                       Error e     -> liftIO (logToStdout ("error in command: " ++ e)) >> loop doCompression
                                       Success cmd -> do 
                                                         stdout_excl <- liftIO $ hDuplicate stdout
                                                         liftIO $ hDuplicateTo stderr stdout  -- redirect stdout to stderr
                                                         (res, continue) <- lift $ executeCommand cmd
                                                         liftIO $ hDuplicateTo stdout_excl stdout  -- redirect stdout to original stdout
                                                         let encoded    = LBS.append (encode res) "\n"
                                                             compressed = if doCompression 
                                                                then Zlib.compressWith Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestSpeed } encoded
                                                                else encoded
                                                         liftIO $ LBS.putStr compressed
                                                         liftIO $ hFlush stdout
                                                         when continue $ loop doCompression

