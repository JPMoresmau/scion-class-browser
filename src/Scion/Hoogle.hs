module Scion.Hoogle
( query
, downloadData
, module Scion.Hoogle.Types
) where

import qualified Data.ByteString.Char8 as BS8
import Scion.Browser
import Scion.Hoogle.Types
import Scion.Hoogle.Instances.Json ()
import Scion.Hoogle.Parser
import Scion.Hoogle.Util
import System.Exit (ExitCode(..))
import System.Process
import Text.Parsec.Prim (runP)

query :: Database -> String -> IO [Result]
query db q = do mpath <- findHoogleBinPath
                case mpath of
                  Nothing   -> return []
                  Just path -> do (exitCode, output, _) <- readProcessWithExitCode path [q] ""
                                  case exitCode of
                                    ExitSuccess -> case runP (hoogleElements db) () "hoogle-output" (BS8.pack output) of
                                                     Right result -> return result
                                                     Left  _      -> return []
                                    _           -> return []

downloadData :: IO Bool
downloadData = do mpath <- findHoogleBinPath
                  case mpath of
                    Nothing   -> return False
                    Just path -> do putStrLn "Running hoogle data..."
                                    (_, _, _) <- readProcessWithExitCode path ["data"] ""
                                    return True

