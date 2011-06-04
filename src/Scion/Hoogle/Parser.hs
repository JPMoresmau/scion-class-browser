{-# LANGUAGE RankNTypes #-}

module Scion.Hoogle.Parser where

import Data.List (intercalate)
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser.Types
import Scion.Browser.Parser.Internal
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data HalfResult = HalfPackage  String
                | HalfModule   String (Documented Module)
                | HalfDecl     String (Documented Decl)
                | HalfGadtDecl String (Documented GadtDecl)

hoogleElements :: BSParser [HalfResult]
hoogleElements =   try (do spaces0
                           eof
                           return [])
               <|> (do first <- hoogleElement
                       rest <- many (try eol >> try hoogleElement)
                       eof
                       return $ first:rest)

hoogleElement :: BSParser HalfResult
hoogleElement =   try (do pname <- hooglePackageName
                          return $ HalfPackage pname)
              <|> try (do (mname, m) <- moduled (module_ NoDoc)
                          return $ HalfModule mname m)
              <|> try (do (mname, d) <- moduled (function NoDoc)
                          return $ HalfDecl mname d)
              <|> try (do (mname, d) <- moduled (dataHead NoDoc)
                          return $ HalfDecl mname d)
              <|> try (do (mname, d) <- moduled (newtypeHead NoDoc)
                          return $ HalfDecl mname d)
              <|> try (do (mname, d) <- moduled (type_ NoDoc)
                          return $ HalfDecl mname d)
              <|> try (do (mname, d) <- moduled (class_ NoDoc)
                          return $ HalfDecl mname d)
              <|> try (do (mname, d) <- moduled (constructor NoDoc)
                          return $ HalfGadtDecl mname d)

moduled :: BSParser a -> BSParser (String, a)
moduled p = try (do mname <- try conid `sepBy` char '.'
                    let name = intercalate "." (map getid mname)
                    try spaces1
                    rest <- p
                    return (name, rest))

hooglePackageName :: BSParser String
hooglePackageName = do string "package"
                       spaces1
                       name <- restOfLine
                       spaces0
                       return name

