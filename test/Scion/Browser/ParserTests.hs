
module Scion.Browser.ParserTests where

import Scion.Browser.Parser
import Scion.Browser.Types
import Scion.Browser.Util
import qualified Data.Map as M
import Test.HUnit
import Language.Haskell.Exts.Annotated.Syntax
import System.Directory
import System.FilePath
import Data.Serialize

parserTests :: [Test]
parserTests = checkValids


checkValids=map (\(f,exps)->TestLabel ("Testing parsing "++f) (TestCase (checkValid f exps))) [
        ("warp",[("Network.Wai.Handler.Warp",["run","resume,pause","Settings","Manager"])])
        ,("wai",[("Network.Wai",[])])
        ,("vector",[("Data.Vector.Storable.Internal",["getPtr"]),("Data.Vector",["Vector","length"])])
        ,("ghc-mtl",[("Control.Monad.Ghc",["runGhc","Ghc"])])
        ,("html",[("Text.Html",["HtmlElement","markupContent"])])
        ,("containers",[("Data.Tree",["Tree","drawTree"])])
        ,("haskell98",[("Maybe",["Maybe","isJust"])])
        ,("haskell2010",[("Data.Array",["Array","ixmap"]),("Data.Complex",["(:+)"])])
        ,("ghc-prim",[])
        ,("base-unicode-symbols",[("Data.Ord.Unicode",["(≯)"])])
        ]

checkValid name exps=do
        let f="data" </> addExtension name "txt"
        fe<-doesFileExist f
        assertBool (f++" does not exist") fe
        res<-parseHoogleFile f
        case res of
                Right p@(Package _ pid m)->do
                        mapM_ (checkPresence m) exps
                        let db=pkgListToDb [p]
                        let bs=encode db
                        case ((decode bs)::Either String Database) of
                             Left _  -> assertFailure "cannot decode db"
                             Right db2 ->do
                                let mp=M.lookup pid db2
                                case mp of
                                        Just (Package _ _ m2) -> mapM_ (checkPresence m2) exps
                                        Nothing -> assertFailure "cannot find pkg"
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
                