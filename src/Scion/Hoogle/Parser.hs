{-# LANGUAGE RankNTypes #-}

module Scion.Hoogle.Parser where

import Data.List (find, intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)
import Distribution.Package hiding (Package)
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser
import Scion.Browser.Parser.Internal
import Scion.Browser.Query
import Scion.Hoogle.Types
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data HalfResult = HalfPackage  String
                | HalfModule   String (Documented Module)
                | HalfDecl     String (Documented Decl)
                | HalfGadtDecl String (Documented GadtDecl)

hoogleElements :: Database -> BSParser [Result]
hoogleElements db = do elts <- hoogleElements'
                       return $ catMaybes $ map (convertHalfToResult db) elts

hoogleElements' :: BSParser [HalfResult]
hoogleElements' =   try (do spaces0
                            eof
                            return [])
                <|> (do first <- hoogleElement
                        rest <- many $ try (try eol >> try hoogleElement)
                        spaces
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

convertHalfToResult :: Database -> HalfResult -> Maybe Result
convertHalfToResult db (HalfPackage  pname)     = case packagesByName pname db of
                                                    []   -> Nothing
                                                    pkgs -> Just $ RPackage pkgs
convertHalfToResult db (HalfModule   mname _)   = case findPackagesForModule db mname of
                                                    []  -> Nothing
                                                    mds -> Just $ RModule mds
convertHalfToResult db (HalfDecl     mname dcl) = let pidMods = findPackagesForModule db mname
                                                  in case findDeclsInModules pidMods dcl of
                                                       []    -> Nothing
                                                       decls -> Just $ RDeclaration decls
convertHalfToResult db (HalfGadtDecl mname dcl) = let pidMods = findPackagesForModule db mname
                                                      gadts = concatMap filterGadtStyleItems pidMods
                                                  in case findConstructorsInGadts gadts (getName dcl) of
                                                       []    -> Nothing
                                                       decls -> Just $ RConstructor decls

findPackagesForModule :: Database -> String -> [(PackageIdentifier, Documented Module)]
findPackagesForModule db md = let pkgs = M.filter (\(Package _ _ mds) -> M.member md mds) db
                              in  M.toAscList $ M.map (\(Package _ _ mds) -> fromJust $ M.lookup md mds) pkgs

findDeclsInModules :: [(PackageIdentifier, Documented Module)] -> Documented Decl -> [(PackageIdentifier, String, Documented Decl)]
findDeclsInModules pidMods declName = foldr (\pidMod lst -> case findDeclInModule pidMod declName of
                                                                   Nothing -> lst
                                                                   Just d  -> d:lst)
                                            [] pidMods

findDeclInModule :: (PackageIdentifier, Documented Module) -> Documented Decl -> Maybe (PackageIdentifier, String, Documented Decl)
findDeclInModule (pid, md@(Module _ _ _ _ decls)) dname = case find (\d -> (fmap (const "") d) == (fmap (const "") dname)) decls of
                                                            Nothing -> Nothing
                                                            Just d  -> Just (pid, getName md, d)
findDeclInModule _ _ = error "The impossible happened"

filterGadtStyleItems :: (PackageIdentifier, Documented Module) -> [(PackageIdentifier, String, Documented Decl)]
filterGadtStyleItems (pid, md@(Module _ _ _ _ decls)) = 
  map (\d -> (pid, getName md, d)) $ filter (\x -> case x of 
                                                     (GDataDecl _ _ _ _ _ _ _) -> True
                                                     _                         -> False)
                                            decls
filterGadtStyleItems _ = error "The impossible happened"

findConstructorsInGadts :: [(PackageIdentifier, String, Documented Decl)] -> String -> [(PackageIdentifier, String, Documented Decl, Documented GadtDecl)]
findConstructorsInGadts gadts conName = foldr (\gadt lst -> case findConstructorInGadt gadt conName of
                                                              Nothing -> lst
                                                              Just c  -> c:lst)
                                              [] gadts

findConstructorInGadt :: (PackageIdentifier, String, Documented Decl) -> String -> Maybe (PackageIdentifier, String, Documented Decl, Documented GadtDecl)
findConstructorInGadt (pid, mdName, gadt@(GDataDecl _ _ _ _ _ decls _)) cname = 
  case find (\c -> (getName c) == cname) decls of
    Nothing -> Nothing
    Just c  -> Just (pid, mdName, gadt, c)
findConstructorInGadt _ _ = error "The impossible happened"

