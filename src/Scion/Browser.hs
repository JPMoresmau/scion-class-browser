{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Scion.Browser where

import Control.Monad (liftM)
import Data.Serialize
import Data.DeriveTH
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Distribution.Package hiding (Package)
import qualified Distribution.Package as P
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import System.IO

-- |Documentation for an item.
-- Now it is simply a Text element.
data Doc = NoDoc
         | Doc T.Text
         deriving Show

docFromString :: String -> Doc
docFromString s = Doc (T.pack s)

-- |A documented item.
type Documented a = a Doc

-- |A package.
data Package l = Package l
                         PackageIdentifier
                         (M.Map String (Documented Module))
               deriving Show

instance P.Package (Package l) where
  packageId (Package _ i _) = i

-- |A Database saves a list of packages. 
type Database = [Documented Package]

type MDatabase = M.Map PackageIdentifier (Documented Package)

saveDatabase :: FilePath -> Database -> IO ()
saveDatabase fpath db  = withFile fpath WriteMode $
                           \hnd -> BS.hPut hnd (encode db)

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase fpath = withFile fpath ReadMode $
                       \hnd -> do s <- BS.hGetContents hnd
                                  return $ case decode s of
                                             Left _  -> Nothing
                                             Right p -> Just p

dbToMDb :: Database -> MDatabase
dbToMDb db = M.fromList (map (\pkg -> (packageId pkg, pkg)) db)

saveMDatabase :: FilePath -> MDatabase -> IO ()
saveMDatabase fpath db  = withFile fpath WriteMode $
                            \hnd -> BS.hPut hnd (encode db)

loadMDatabase :: FilePath -> IO (Maybe MDatabase)
loadMDatabase fpath = withFile fpath ReadMode $
                        \hnd -> do s <- BS.hGetContents hnd
                                   return $ case decode s of
                                              Left _  -> Nothing
                                              Right p -> Just p

-- Binary instances for different elements

instance Serialize T.Text where
  put = put . E.encodeUtf8
  get = liftM E.decodeUtf8 get

$( derive makeSerialize ''Doc )
$( derive makeSerialize ''Package )

-- derive Binary instances for Cabal packages
$( derive makeSerialize ''PackageIdentifier )
$( derive makeSerialize ''PackageName )
$( derive makeSerialize ''Version )
  
-- derive Binary instances for haskell-src-exts

noDoc :: Doc
noDoc = NoDoc

nothing :: Maybe a
nothing = Nothing

instance Serialize (Documented Module) where
  -- Only possible value
  -- Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
  put (Module doc (Just head) _ _ decls) = do put doc
                                              put head
                                              put decls
  put _                                  = error "Not allowed Module"
  get = do doc <- get
           head <- get
           decls <- get
           return $ Module doc (Just head) [] [] decls

instance Serialize (Documented ModuleHead) where
  -- Only possible value
  -- ModuleHead l (ModuleName l) (Maybe (WarningText l)) (Maybe (ExportSpecList l))
  put (ModuleHead _ (ModuleName _ name) _ _) = do put name
  get = do name <- get
           return $ ModuleHead noDoc (ModuleName noDoc name) nothing nothing

dataType :: Documented DataOrNew
dataType = DataType noDoc

newType :: Documented DataOrNew
newType = NewType noDoc

instance Serialize (Documented Decl) where
  -- Possible values
  -- GDataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l))
  -- ClassDecl l (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l])
  -- InstDecl l (Maybe (Context l)) (InstHead l) (Maybe [InstDecl l])
  -- TypeSig l [Name l] (Type l)
  -- TypeDecl l (DeclHead l) (Type l)
  put (GDataDecl doc dOrM ctx head kind decls _) = do put doc
                                                      putWord8 0
                                                      case dOrM of
                                                        DataType _ -> putWord8 0
                                                        NewType _  -> putWord8 1
                                                      put ctx
                                                      put head
                                                      put kind
                                                      put decls
  put (ClassDecl doc ctx head fdeps _) = do put doc
                                            putWord8 1
                                            put ctx
                                            put head
                                            put fdeps
  put (InstDecl doc ctx head _) = do put doc
                                     putWord8 2
                                     put ctx
                                     put head
  put (TypeSig doc [name] ty) = do put doc
                                   putWord8 3
                                   put name
                                   put ty
  put (TypeDecl doc head ty) = do put doc
                                  putWord8 4
                                  put head
                                  put ty
  put _ = error "Not allowed Decl"
  get = do doc <- get
           tag <- getWord8
           case tag of
             0 -> do dOrM' <- getWord8
                     let dOrM = case dOrM' of
                                  0 -> dataType
                                  1 -> newType
                     ctx <- get
                     head <- get
                     kind <- get
                     decls <- get
                     return $ GDataDecl doc dOrM ctx head kind decls nothing
             1 -> do ctx <- get
                     head <- get
                     fdeps <- get
                     return $ ClassDecl doc ctx head fdeps nothing
             2 -> do ctx <- get
                     head <- get
                     return $ InstDecl doc ctx head nothing
             3 -> do name <- get
                     ty <- get
                     return $ TypeSig doc [name] ty
             _ -> do head <- get
                     ty <- get
                     return $ TypeDecl doc head ty

cxEmpty :: Documented Context
cxEmpty = CxEmpty noDoc

instance Serialize (Documented Context) where
  -- Possible values
  -- CxSingle l (Asst l)	 
  -- CxTuple l [Asst l]	 
  -- CxParen l (Context l)	 
  -- CxEmpty l
  put (CxSingle _ a)  = put [a]
  put (CxTuple _ as)  = put as
  put (CxParen _ ctx) = put ctx
  put (CxEmpty _)     = put ([] :: [Documented Asst])
  get = do (as :: [Documented Asst]) <- get
           return $ case as of
                      []  -> cxEmpty
                      [a] -> CxSingle noDoc a
                      as  -> CxTuple noDoc as

instance Serialize (Documented Asst) where
  -- Possible values
  -- ClassA l (QName l) [Type l]
  -- InfixA l (Type l) (QName l) (Type l)	
  -- IParam l (IPName l) (Type l)	
  -- EqualP l (Type l) (Type l)
  put (ClassA _ name tys) = do putWord8 0
                               put name
                               put tys
  put (InfixA _ ty1 name ty2) = do putWord8 1
                                   put ty1
                                   put name
                                   put ty2
  put (IParam _ ipname ty) = do putWord8 2
                                put ipname
                                put ty
  put (EqualP _ ty1 ty2) = do putWord8 3
                              put ty1
                              put ty2
  get = do tag <- getWord8
           case tag of
             0 -> do name <- get
                     tys <- get
                     return $ ClassA noDoc name tys
             1 -> do ty1 <- get
                     name <- get
                     ty2 <- get
                     return $ InfixA noDoc ty1 name ty2
             2 -> do ipname <- get
                     ty <- get
                     return $ IParam noDoc ipname ty
             _ -> do ty1 <- get
                     ty2 <- get
                     return $ EqualP noDoc ty1 ty2

instance Serialize (Documented DeclHead) where
  -- Only possible value
  -- DHead l (Name l) [TyVarBind l]
  put (DHead _ name vars) = do put name
                               put vars
  put _ = error "Not allowed DeclHead"
  get = do name <- get
           vars <- get
           return $ DHead noDoc name vars

instance Serialize (Documented TyVarBind) where
  -- Possible values
  -- KindedVar l (Name l) (Kind l)
  -- UnkindedVar l (Name l)
  put (KindedVar _ name kind) = do put name
                                   putWord8 0
                                   put kind
  put (UnkindedVar _ name) = do put name
                                putWord8 1
  get = do name <- get
           tag <- getWord8
           case tag of
             0 -> do kind <- get
                     return $ KindedVar noDoc name kind
             _ -> return $ UnkindedVar noDoc name

kindStar :: Documented Kind
kindStar = KindStar noDoc

kindBang :: Documented Kind
kindBang = KindBang noDoc

instance Serialize (Documented Kind) where
  -- Possible values
  -- KindStar l
  -- KindBang l
  -- KindFn l (Kind l) (Kind l)
  -- KindParen l (Kind l)
  -- KindVar l (Name l)
  put (KindStar _)     = putWord8 0
  put (KindBang _)     = putWord8 1
  put (KindFn _ k1 k2) = do putWord8 2
                            put k1
                            put k2
  put (KindParen _ k)  = do putWord8 3
                            put k
  put (KindVar _ name) = do putWord8 4
                            put name
  get = do tag <- getWord8
           case tag of
             0 -> return kindStar
             1 -> return kindBang
             2 -> do k1 <- get
                     k2 <- get
                     return $ KindFn noDoc k1 k2
             3 -> do k <- get
                     return $ KindParen noDoc k
             _ -> do name <- get
                     return $ KindVar noDoc name

instance Serialize (Documented FunDep) where
  -- Only possible value
  -- FunDep l [Name l] [Name l]
  put (FunDep _ n1 n2) = do put n1
                            put n2
  get = do n1 <- get
           n2 <- get
           return $ FunDep noDoc n1 n2

instance Serialize (Documented GadtDecl) where
  -- Only possible value
  -- GadtDecl l (Name l) (Type l)
  put (GadtDecl _ name ty) = do put name
                                put ty
  get = do name <- get
           ty <- get
           return $ GadtDecl noDoc name ty

instance Serialize (Documented InstHead) where
  -- Only possible value
  -- IHead l (QName l) [Type l]
  put (IHead _ qname tys) = do put qname
                               put tys
  put _ = error "Not allowed IHead"
  get = do qname <- get
           tys <- get
           return $ IHead noDoc qname tys

instance Serialize (Documented Type) where
  -- Possible values
  -- TyForall l (Maybe [TyVarBind l]) (Maybe (Context l)) (Type l)
  -- TyFun l (Type l) (Type l)
  -- TyTuple l Boxed [Type l]
  -- TyList l (Type l)
  -- TyApp l (Type l) (Type l)
  -- TyVar l (Name l)
  -- TyCon l (QName l)
  -- TyParen l (Type l)
  -- TyInfix l (Type l) (QName l) (Type l)
  -- TyKind l (Type l) (Kind l)
  put (TyForall _ vars ctx ty) = do putWord8 0
                                    put vars
                                    put ctx
                                    put ty
  put (TyFun _ ty1 ty2) = do putWord8 1
                             put ty1
                             put ty2
  put (TyTuple _ boxed tys) = do putWord8 2
                                 put boxed
                                 put tys
  put (TyList _ ty) = do putWord8 3
                         put ty
  put (TyApp _ ty1 ty2) = do putWord8 4
                             put ty1
                             put ty2
  put (TyVar _ name) = do putWord8 5
                          put name
  put (TyCon _ qname) = do putWord8 6
                           put qname
  put (TyParen _ ty) = do putWord8 7
                          put ty
  put (TyInfix _ ty1 qname ty2) = do putWord8 8
                                     put ty1
                                     put qname
                                     put ty2
  put (TyKind _ ty k) = do putWord8 9
                           put ty
                           put k
  get = do tag <- getWord8
           case tag of
             0 -> do vars <- get
                     ctx <- get
                     ty <- get
                     return $ TyForall noDoc vars ctx ty
             1 -> do ty1 <- get
                     ty2 <- get
                     return $ TyFun noDoc ty1 ty2
             2 -> do boxed <- get
                     tys <- get
                     return $ TyTuple noDoc boxed tys
             3 -> do ty <- get
                     return $ TyList noDoc ty
             4 -> do ty1 <- get
                     ty2 <- get
                     return $ TyApp noDoc ty1 ty2
             5 -> do name <- get
                     return $ TyVar noDoc name
             6 -> do qname <- get
                     return $ TyCon noDoc qname
             7 -> do ty <- get
                     return $ TyParen noDoc ty
             8 -> do ty1 <- get
                     qname <- get
                     ty2 <- get
                     return $ TyInfix noDoc ty1 qname ty2
             _ -> do ty <- get
                     k <- get
                     return $ TyKind noDoc ty k

boxed_ :: Boxed
boxed_ = Boxed

unboxed_ :: Boxed
unboxed_ = Unboxed

instance Serialize Boxed where
  -- Possible values
  -- Boxed
  -- Unboxed
  put Boxed  = putWord8 0
  put Unboxed = putWord8 1
  get = do tag <- getWord8
           return $ case tag of
                      0 -> boxed_
                      _ -> unboxed_

instance Serialize (Documented Name) where
  -- Possible values
  -- Ident l String
  -- Symbol l String
  put (Ident _ s)  = do putWord8 0
                        put s
  put (Symbol _ s) = do putWord8 1
                        put s
  get = do tag <- getWord8
           s <- get
           case tag of
             0 -> return $ Ident noDoc s
             _ -> return $ Symbol noDoc s

instance Serialize (Documented QName) where
  -- Possible values
  -- Qual l (ModuleName l) (Name l)
  -- UnQual l (Name l)
  -- Special l (SpecialCon l)
  put (Qual _ (ModuleName _ mn) name) = do putWord8 0
                                           put mn
                                           put name
  put (UnQual _ name) = do putWord8 1
                           put name
  put (Special _ scon) = do putWord8 2
                            put scon
  get = do tag <- getWord8
           case tag of
             0 -> do mn <- get
                     name <- get
                     return $ Qual noDoc (ModuleName noDoc mn) name
             1 -> do name <- get
                     return $ UnQual noDoc name
             _ -> do scon <- get
                     return $ Special noDoc scon

instance Serialize (Documented IPName) where
  -- Possible values
  -- IPDup l String
  -- IPLin l String
  put (IPDup _ s) = do put s
                       putWord8 0
  put (IPLin _ s) = do put s
                       putWord8 1
  get = do s <- get
           tag <- getWord8
           case tag of
             0 -> return $ IPDup noDoc s
             _ -> return $ IPLin noDoc s

unitCon :: Documented SpecialCon
unitCon = UnitCon noDoc

listCon :: Documented SpecialCon
listCon = ListCon noDoc

funCon :: Documented SpecialCon
funCon = FunCon noDoc

cons_ :: Documented SpecialCon
cons_ = Cons noDoc

unboxedSingleCon :: Documented SpecialCon
unboxedSingleCon = UnboxedSingleCon noDoc

instance Serialize (Documented SpecialCon) where
  -- Possible values
  -- UnitCon l
  -- ListCon l
  -- FunCon l
  -- Cons l
  -- UnboxedSingleCon l
  -- TupleCon l Boxed Int
  put (UnitCon _) = putWord8 0
  put (ListCon _) = putWord8 1
  put (FunCon _)  = putWord8 2
  put (Cons _)    = putWord8 3
  put (UnboxedSingleCon _) = putWord8 4
  put (TupleCon _ boxed n) = do putWord8 5
                                put boxed
                                put n
  get = do tag <- getWord8
           case tag of
             0 -> return unitCon
             1 -> return listCon
             2 -> return funCon
             3 -> return cons_
             4 -> return unboxedSingleCon
             _ -> do boxed <- get
                     n <- get
                     return $ TupleCon noDoc boxed n

