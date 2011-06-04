module Scion.Browser
( saveDatabase
, loadDatabase
, singletonDatabase
, module Scion.Browser.Types
, module Scion.Browser.Instances.Json
-- , module Scion.Browser.Instances.NFData
, module Scion.Browser.Instances.Serialize
) where

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Serialize
import Scion.Browser.Types
import Scion.Browser.Instances.Json
import Scion.Browser.Instances.NFData ()
import Scion.Browser.Instances.Serialize
import System.IO

saveDatabase :: FilePath -> Database -> IO ()
saveDatabase fpath db = withFile fpath WriteMode $
                          \hnd -> do BS.hPut hnd (encode db)
                                     hFlush hnd

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase fpath = withFile fpath ReadMode $
                       \hnd -> do s <- BS.hGetContents hnd
                                  return $ case decode s of
                                             Left _  -> Nothing
                                             Right p -> p `deepseq` Just p

singletonDatabase :: Documented Package -> Database
singletonDatabase pkg@(Package _ pid _) = M.singleton pid pkg

