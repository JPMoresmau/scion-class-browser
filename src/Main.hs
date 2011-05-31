module Main where

import Control.Monad.State
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Scion.Browser.Json.Commands
import System.Console.Editline

main :: IO ()
main = do el <- elInit "hoogle-parsec"
          setPrompt el (return ">> ")
          setEditor el Vi
          runStateT (loop el) initialState
          return ()

loop :: EditLine -> BrowserM ()
loop el = do maybeLine <- lift $ elGets el
             case maybeLine of
               Nothing -> return () -- ctrl+D or EOF
               Just line -> do
                 let line' = init line -- remove trailing '\n'
                 case Atto.parse json (BS.pack line') of
                   Atto.Fail _ _ e   -> lift $ putStrLn ("error in command: " ++ e)
                   Atto.Done _ value -> case T.parse parseJSON value of
                                          Error e     -> lift $ putStrLn ("error in command: " ++ e)
                                          Success cmd -> do res <- executeCommand cmd
                                                            lift $ putStrLn $ LBS.unpack (encode res)
             loop el

