
import Scion.Browser.ParserTests

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

main = defaultMain tests

tests = [testGroup "Parser Tests" (concatMap (hUnitTestToTests) parserTests)]