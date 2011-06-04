module Main where

import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Server.Commands
import System.Console.Haskeline

import Scion.Hoogle

main :: IO ()
main = do db <- findHoogleBinPath
          putStrLn $ show db

main' :: IO ()
main' = do runStateT (runInputT defaultSettings loop) initialState
           return ()

loop :: InputT BrowserM ()
loop = do maybeLine <- getInputLine ">> "
          case maybeLine of
            Nothing -> return () -- ctrl+D or EOF
            Just line -> do
              case Atto.parse json (BS.pack line) of
                Atto.Fail _ _ e   -> outputStrLn ("error in command: " ++ e)
                Atto.Done _ value -> case T.parse parseJSON value of
                                       Error e     -> outputStrLn ("error in command: " ++ e)
                                       Success cmd -> do res <- lift $ executeCommand cmd
                                                         outputStrLn $ LBS.unpack (encode res)
          loop

