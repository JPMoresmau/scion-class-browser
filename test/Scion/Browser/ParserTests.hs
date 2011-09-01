
module Scion.Browser.ParserTests where

import Scion.Browser.Parser
import Scion.Browser.Types
import qualified Data.Map as M
import Test.HUnit
import Language.Haskell.Exts.Annotated.Syntax
import System.Directory
import System.FilePath

parserTests :: [Test]
parserTests = checkValids


checkValids=map (\(f,exps)->TestLabel ("Testing parsing "++f) (TestCase (checkValid f exps))) [
        ("warp",[("Network.Wai.Handler.Warp",["run","resume,pause","Settings"])])
        ,("wai",[("Network.Wai",[])])
        ,("vector",[("Data.Vector.Storable.Internal",["getPtr"]),("Data.Vector",["Vector","length"])])
        ,("ghc-mtl",[("Control.Monad.Ghc",["runGhc","Ghc"])])
        ,("html",[("Text.Html",["HtmlElement","markupContent"])])
        ]

checkValid name exps=do
        let f="data" </> addExtension name "txt"
        fe<-doesFileExist f
        assertBool (f++" does not exist") fe
        res<-parseHoogleFile f
        case res of
                Right (Package _ _ m)->do
                        mapM_ (checkPresence m) exps
                        return ()
                Left e->assertFailure $ show e

checkPresence :: (M.Map String (Documented Module)) -> (String,[String]) -> IO()
checkPresence m (modName,exps)=do
        let mmod=M.lookup modName m
        case mmod of
                Nothing->assertFailure ("module not found:" ++ modName)
                Just (Module _ _ _ _ decls)->do
                        let names=map getName decls
                        mapM_ (\e->assertBool e (elem e names)) exps
                        return ()
                