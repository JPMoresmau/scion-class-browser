{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Scion.Browser.Instances.Serialize where

import Control.DeepSeq
import Control.Monad (liftM)
import Data.DeriveTH
import Data.Serialize
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Distribution.Package hiding (Package)
import Distribution.Version
import Language.Haskell.Exts.Annotated.Syntax
import Scion.Browser.Types
import Scion.Browser.Instances.NFData ()
import System.IO.Unsafe

$( derive makeSerialize ''Doc )
$( derive makeSerialize ''Package )
$( derive makeSerialize ''PackageIdentifier )
$( derive makeSerialize ''PackageName )
$( derive makeSerialize ''Version )

instance Serialize T.Text where
  put = put . E.encodeUtf8
  get = liftM E.decodeUtf8 get

-- Lookup table (yes, it uses unsafePerformIO)

lookupNameTable :: IORef (M.Map String (Documented Name))
lookupNameTable = unsafePerformIO $ newIORef M.empty

getNameInLookupTable :: String -> Bool {- is symbol? -} -> Documented Name
getNameInLookupTable name isSymbol = 
  unsafePerformIO $ do table <- readIORef lookupNameTable
                       case M.lookup name table of
                         Just v  -> return v
                         Nothing -> do let element = if isSymbol 
                                                        then (Symbol noDoc name)
                                                        else (Ident noDoc name)
                                       modifyIORef lookupNameTable (M.insert name element)
                                       return element

lookupQNameTable :: IORef (M.Map String (Documented QName))
lookupQNameTable = unsafePerformIO $ newIORef M.empty

getQNameInLookupTable :: Documented QName -> Documented QName
getQNameInLookupTable qname = 
  unsafePerformIO $ do table <- readIORef lookupQNameTable
                       let rname = getQNameString qname
                       case M.lookup rname table of
                         Just v  -> return v
                         Nothing -> do modifyIORef lookupQNameTable (M.insert rname qname)
                                       return qname

lookupTyVarTable :: IORef (M.Map String (Documented Type))
lookupTyVarTable = unsafePerformIO $ newIORef M.empty

getTyVarInLookupTable :: Documented Name -> Documented Type
getTyVarInLookupTable name = 
  unsafePerformIO $ do table <- readIORef lookupTyVarTable
                       let rname = getNameString name
                       case M.lookup rname table of
                         Just v  -> return v
                         Nothing -> do let element = TyVar noDoc name 
                                       modifyIORef lookupTyVarTable (M.insert rname element)
                                       return element

lookupTyConTable :: IORef (M.Map String (Documented Type))
lookupTyConTable = unsafePerformIO $ newIORef M.empty

getTyConInLookupTable :: Documented QName -> Documented Type
getTyConInLookupTable name = 
  unsafePerformIO $ do table <- readIORef lookupTyConTable
                       let rname = getQNameString name
                       case M.lookup rname table of
                         Just v  -> return v
                         Nothing -> do let element = TyCon noDoc name 
                                       modifyIORef lookupTyConTable (M.insert rname element)
                                       return element

-- Serialize instances for haskell-src-exts

noDoc :: Doc
noDoc = NoDoc

nothing :: Maybe a
nothing = Nothing

instance Serialize (Documented Module) where
  -- Only possible value
  -- Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
  put (Module doc (Just hd) _ _ decls) = do put doc
                                            put hd
                                            put decls
  put _                                = error "Not allowed Module"
  get = do doc <- get
           hd <- get
           decls <- get
           return $ decls `deepseq` Module doc (Just hd) [] [] decls

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
  put (GDataDecl doc dOrM ctx hd kind decls _) = do put doc
                                                    putWord8 0
                                                    case dOrM of
                                                      DataType _ -> putWord8 0
                                                      NewType _  -> putWord8 1
                                                    put ctx
                                                    put hd
                                                    put kind
                                                    put decls
  put (ClassDecl doc ctx hd fdeps _) = do put doc
                                          putWord8 1
                                          put ctx
                                          put hd
                                          put fdeps
  put (InstDecl doc ctx hd _) = do put doc
                                   putWord8 2
                                   put ctx
                                   put hd
  put (TypeSig doc [name] ty) = do put doc
                                   putWord8 3
                                   put name
                                   put ty
  put (TypeDecl doc hd ty) = do put doc
                                putWord8 4
                                put hd
                                put ty
  put _ = error "Not allowed Decl"
  get = do doc <- get
           tag <- getWord8
           case tag of
             0 -> do dOrM' <- getWord8
                     let dOrM = case dOrM' of
                                  0 -> dataType
                                  _ -> newType
                     ctx <- get
                     hd <- get
                     kind <- get
                     decls <- get
                     return $ GDataDecl doc dOrM ctx hd kind decls nothing
             1 -> do ctx <- get
                     hd <- get
                     fdeps <- get
                     return $ ClassDecl doc ctx hd fdeps nothing
             2 -> do ctx <- get
                     hd <- get
                     return $ InstDecl doc ctx hd nothing
             3 -> do name <- get
                     ty <- get
                     return $ TypeSig doc [name] ty
             _ -> do hd <- get
                     ty <- get
                     return $ TypeDecl doc hd ty

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
                      ass -> CxTuple noDoc ass

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
                     return $ getTyVarInLookupTable name
             6 -> do qname <- get
                     return $ getTyConInLookupTable qname
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
           let isSymbol = tag /= 0
           return $ getNameInLookupTable s isSymbol

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
                     return $ getQNameInLookupTable $ Qual noDoc (ModuleName noDoc mn) name
             1 -> do name <- get
                     return $ getQNameInLookupTable $ UnQual noDoc name
             _ -> do scon <- get
                     return $ getQNameInLookupTable $ Special noDoc scon

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

