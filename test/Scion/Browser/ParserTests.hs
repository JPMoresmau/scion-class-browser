
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
import Data.List
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Extension
import Data.List.Split
--import Scion.Browser.FileUtil

parserTests :: [Test]
parserTests = checkTypeParse:checkValids

checkValids :: [Test]
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
        ,("base-unicode-symbols",[("Data.Ord.Unicode",["(≯)"]),("Control.Arrow.Unicode",["(⋙)"])])
        ]

checkValid :: String -> [(String,[String])] -> IO()
checkValid name exps=do
        let f="data" </> addExtension name "txt"
        fe<-doesFileExist f
        assertBool (f++" does not exist") fe
        --Just txt<-downloadHoogleFile "http://hackage.haskell.org/packages/archive/warp/0.4.4/doc/html/warp.txt"
        --
        --let res=parseHoogleString "<package>" txt
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
                        let res=A.toJSON decls
                        let output=LBS.toString (A.encode res)
                        assertBool modName (not $ isInfixOf "not parsed" output)
                        mapM_ (\e->mapM_ (\e2->assertBool e2 (isInfixOf e2 output))(splitOn "," e)) exps
                        return ()
      
checkTypeParse :: Test
checkTypeParse=  TestLabel "Testing checkTypeParse" (TestCase (do
        let parseString="Category (⇝) => (α ⇝ β) -> (β ⇝ γ) -> (α ⇝ γ)"     -- does not work if I remove the brackets around the first squiggly arrow
        let parseTypeMode=Parser.ParseMode "" knownExtensions False False Nothing
        let parsed = Parser.parseTypeWithMode parseTypeMode parseString   
        case parsed of
            Parser.ParseFailed _ msg -> assertFailure msg
            Parser.ParseOk _ -> return ()
        ))  