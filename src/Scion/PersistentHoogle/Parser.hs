{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

module Scion.PersistentHoogle.Parser where

import Control.Monad (filterM)
import Data.List (intercalate)
import Database.Persist
import Database.Persist.Sqlite
import Language.Haskell.Exts.Annotated.Syntax
import Scion.PersistentBrowser.DbTypes
import Scion.PersistentBrowser.Parser.Internal
import Scion.PersistentBrowser.Query
import Scion.PersistentBrowser.ToDb
import Scion.PersistentBrowser.Types
import Scion.PersistentHoogle.Types
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data HalfResult = HalfPackage  String
                | HalfModule   String (Documented Module)
                | HalfDecl     String (Documented Decl)
                | HalfGadtDecl String (Documented GadtDecl)

hoogleElements :: BSParser (SqlPersist IO [Result])
hoogleElements = do elts <- hoogleElements'
                    let results = catMaybesM $ map convertHalfToResult elts
                    return results

catMaybesM :: Monad m => [m (Maybe a)] -> m [a]
catMaybesM []     = return []
catMaybesM (x:xs) = do y <- x
                       zs <- catMaybesM xs
                       case y of
                         Nothing -> return zs
                         Just z  -> return (z:zs)

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

convertHalfToResult :: HalfResult -> SqlPersist IO (Maybe Result)
convertHalfToResult (HalfPackage  pname) = 
  do pkgs <- packagesByName pname
     case pkgs of
       [] -> return Nothing
       p  -> return $ Just (RPackage p)
convertHalfToResult (HalfModule mname _) =
  do mods <- modulesByName mname
     case mods of
       [] -> return Nothing
       m  -> do pm <- mapM (\md -> do pkg <- getDbPackage md
                                      return (dbPackageToIdentifier pkg, md)) m
                return $ Just (RModule pm)
convertHalfToResult (HalfDecl mname dcl) =
  -- TODO: Check the rest of the things
  do decls <- selectList [ DbDeclName ==. (getName dcl) ] []
     filteredDecls <- filterM (\(_, dc) -> do (DbModule mn _ _) <- getDbModule dc
                                              return $ mn == mname) decls
     case filteredDecls of
       [] -> return Nothing
       d  -> do dm <- mapM (\(declId, dc) -> do md@(DbModule mn _ _) <- getDbModule dc
                                                pkg <- getDbPackage md
                                                completeDecl <- getAllDeclInfo (declId, dc)
                                                return (dbPackageToIdentifier pkg, mn, completeDecl)) d
                return $ Just (RDeclaration dm)
convertHalfToResult (HalfGadtDecl mname dcl) =
  do consts <- constructorsByName (getName dcl)
     filteredConsts <- filterM (\dc -> do (DbModule mn _ _) <- getDbModule dc
                                          return $ mn == mname) consts
     case filteredConsts of
       [] -> return Nothing
       c  -> do dm <- mapM (\ct@(DbConstructor _ _ declId) -> 
                                  do Just dc <- get declId
                                     completeDecl <- getAllDeclInfo (declId, dc)
                                     md@(DbModule mn _ _) <- getDbModule dc
                                     pkg <- getDbPackage md
                                     return (dbPackageToIdentifier pkg, mn, completeDecl, ct)) c
                return $ Just (RConstructor dm)

checkEqualInDb :: Documented Decl -> DbCompleteDecl -> Bool
checkEqualInDb (GDataDecl _ (DataType _) ctx (DHead _ name vars) knd _ _) (DbCompleteDecl (DbDecl DbData dbName _ dbKind _ _ _) dbCtx dbVars _ _) =
     (getNameString name) == dbName
  && (map singleLinePrettyPrint vars) == (map (\(DbTyVar vn _) -> vn) dbVars)
  && (contextToDb (maybeEmptyContext ctx)) == (map (\(DbContext s _) -> s) dbCtx)
  && (fmap singleLinePrettyPrint knd) == dbKind
checkEqualInDb (GDataDecl _ (NewType _) ctx (DHead _ name vars) knd _ _) (DbCompleteDecl (DbDecl DbNewType dbName _ dbKind _ _ _) dbCtx dbVars _ _) =
     (getNameString name) == dbName
  && (map singleLinePrettyPrint vars) == (map (\(DbTyVar vn _) -> vn) dbVars)
  && (contextToDb (maybeEmptyContext ctx)) == (map (\(DbContext s _) -> s) dbCtx)
  && (fmap singleLinePrettyPrint knd) == dbKind
checkEqualInDb (ClassDecl _ ctx (DHead _ name vars) fdeps _) (DbCompleteDecl (DbDecl DbClass dbName _ _ _ _ _) dbCtx dbVars dbFunDeps _) =
     (getNameString name) == dbName
  && (map singleLinePrettyPrint vars) == (map (\(DbTyVar vn _) -> vn) dbVars)
  && (contextToDb (maybeEmptyContext ctx)) == (map (\(DbContext s _) -> s) dbCtx)
  && (map singleLinePrettyPrint fdeps) == (map (\(DbFunDep fd _) -> fd) dbFunDeps)
checkEqualInDb (InstDecl _ ctx (IHead _ name vars) _) (DbCompleteDecl (DbDecl DbInstance dbName _ _ _ _ _) dbCtx dbVars _ _) =
     (getQNameString name) == dbName
  && (map singleLinePrettyPrint vars) == (map (\(DbTyVar vn _) -> vn) dbVars)
  && (contextToDb (maybeEmptyContext ctx)) == (map (\(DbContext s _) -> s) dbCtx)
checkEqualInDb (TypeSig _ [ name ] ty) (DbCompleteDecl (DbDecl DbSignature dbName _ _ dbSignature _ _) _ _ _ _) =
     (getNameString name) == dbName
  && Just (singleLinePrettyPrint ty) == dbSignature
checkEqualInDb (TypeDecl _ (DHead _ name vars) ty) (DbCompleteDecl (DbDecl DbType dbName _ _ _ dbEquals _) _ dbVars _ _) =
     (getNameString name) == dbName
  && (map singleLinePrettyPrint vars) == (map (\(DbTyVar vn _) -> vn) dbVars)
  && Just (singleLinePrettyPrint ty) == dbEquals
checkEqualInDb _ _ = False

