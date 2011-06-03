{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

-- Instances of Serialize and NFData
-- for types in haskell-src-exts

module Scion.Browser.HSEInstances where

import Control.DeepSeq
import Control.Monad (liftM)
import Data.Serialize
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Language.Haskell.Exts.Annotated.Syntax
import System.IO.Unsafe

-- |Documentation for an item.
-- Now it is simply a Text element.
data Doc = NoDoc
         | Doc T.Text
         deriving Show

docFromString :: String -> Doc
docFromString s = Doc (T.pack s)

-- |A documented item.
type Documented a = a Doc

-- ----------------------------------
-- Instances for Serialize and NFData
-- for a bunch of datatypes
-- ----------------------------------

instance Serialize T.Text where
  put = put . E.encodeUtf8
  get = liftM E.decodeUtf8 get

-- |Gets the name inside a Name constructor.
getNameString :: Name l -> String
getNameString (Ident _ s)  = s
getNameString (Symbol _ s) = "(" ++ s ++ ")"

-- |Gets the qualified name as a string.
getQNameString :: QName l -> String
getQNameString (Qual _ (ModuleName _ "")    ename) = getNameString ename
getQNameString (Qual _ (ModuleName _ mname) ename) = mname ++ "." ++ getNameString ename
getQNameString (UnQual _ ename)                    = getNameString ename
getQNameString (Special _ (UnitCon _))             = "()"
getQNameString (Special _ (ListCon _))             = "[]"
getQNameString (Special _ (FunCon _))              = "(->)"
getQNameString (Special _ (TupleCon _ box n))      = case box of
                                                       Boxed   -> "(" ++ replicate (n-1) ',' ++ ")"
                                                       Unboxed -> "(#" ++ replicate (n-1) ',' ++ "#)"
getQNameString (Special _ (Cons _))                = "(:)"
getQNameString (Special _ (UnboxedSingleCon _))    = "(# #)"

-- Lookup table (yes, it used unsafePerformIO)

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


-- The following code was autogenerated using Template Haskell
-- If some element changes, rerun ghc -ddump-splices file.hs

instance Serialize Doc where
  put x = case x of
            NoDoc  -> putWord8 0
            Doc x1 -> do putWord8 1
                         put x1
  get = do i <- getWord8;
           case i of
             0 -> return NoDoc
             1 -> do x1 <- get
                     return (Doc x1)
             _ -> error "Corrupted binary data for Doc"


instance NFData Doc where
  rnf NoDoc    = ()
  rnf (Doc x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (Module l) where
  rnf (Module x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (XmlPage x1 x2 x3 x4 x5 x6 x7) 
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` (rnf x7 `seq` ())))))))
  rnf (XmlHybrid x1 x2 x3 x4 x5 x6 x7 x8 x9)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` (rnf x7 `seq` (rnf x8 `seq` (rnf x9 `seq` ()))))))))) 

instance NFData l => NFData (ModuleHead l) where
  rnf (ModuleHead x1 x2 x3 x4) = 
    (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ())))) 

instance NFData l => NFData (WarningText l) where
  rnf (DeprText x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (WarnText x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (ExportSpecList l) where
  rnf (ExportSpecList x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (ExportSpec l) where
  rnf (EVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (EAbs x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (EThingAll x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (EThingWith x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (EModuleContents x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (ImportDecl l) where
  rnf (ImportDecl x1 x2 x3 x4 x5 x6 x7)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` (rnf x7 `seq` ()))))))) 

instance NFData l => NFData (ImportSpecList l) where
  rnf (ImportSpecList x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (ImportSpec l) where
  rnf (IVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IAbs x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IThingAll x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IThingWith x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Assoc l) where
  rnf (AssocNone x1) = (rnf x1 `seq` ())
  rnf (AssocLeft x1) = (rnf x1 `seq` ())
  rnf (AssocRight x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (Decl l) where
  rnf (TypeDecl x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (TypeFamDecl x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (DataDecl x1 x2 x3 x4 x5 x6) 
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ()))))))
  rnf (GDataDecl x1 x2 x3 x4 x5 x6 x7)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` (rnf x7 `seq` ())))))))
  rnf (DataFamDecl x1 x2 x3 x4) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (TypeInsDecl x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (DataInsDecl x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (GDataInsDecl x1 x2 x3 x4 x5 x6)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ()))))))
  rnf (ClassDecl x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (InstDecl x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (DerivDecl x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (InfixDecl x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (DefaultDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (SpliceDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TypeSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (FunBind x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PatBind x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (ForImp x1 x2 x3 x4 x5 x6)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ()))))))
  rnf (ForExp x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (RulePragmaDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (DeprPragmaDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (WarnPragmaDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (InlineSig x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (InlineConlikeSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (SpecSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (SpecInlineSig x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (InstSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (AnnPragma x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (DeclHead l) where
  rnf (DHead x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (DHInfix x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (DHParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (InstHead l) where
  rnf (IHead x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (IHInfix x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (IHParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Binds l) where
  rnf (BDecls x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IPBinds x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (IPBind l) where
  rnf (IPBind x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (ClassDecl l) where
  rnf (ClsDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ClsDataFam x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (ClsTyFam x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (ClsTyDef x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (InstDecl l) where
  rnf (InsDecl x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (InsType x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (InsData x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (InsGData x1 x2 x3 x4 x5 x6)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ())))))) 

instance NFData l => NFData (Deriving l) where
  rnf (Deriving x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (DataOrNew l) where
  rnf (DataType x1) = (rnf x1 `seq` ())
  rnf (NewType x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (ConDecl l) where
  rnf (ConDecl x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (InfixConDecl x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (RecDecl x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (FieldDecl l) where
  rnf (FieldDecl x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (QualConDecl l) where
  rnf (QualConDecl x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ())))) 

instance NFData l => NFData (GadtDecl l) where
  rnf (GadtDecl x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (BangType l) where
  rnf (BangedTy x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (UnBangedTy x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (UnpackedTy x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Match l) where
  rnf (Match x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (InfixMatch x1 x2 x3 x4 x5 x6)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ())))))) 

instance NFData l => NFData (Rhs l) where
  rnf (UnGuardedRhs x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (GuardedRhss x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (GuardedRhs l) where
  rnf (GuardedRhs x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Context l) where
  rnf (CxSingle x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (CxTuple x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (CxParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (CxEmpty x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (FunDep l) where
  rnf (FunDep x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Asst l) where
  rnf (ClassA x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (InfixA x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (IParam x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (EqualP x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Type l) where
  rnf (TyForall x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (TyFun x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (TyTuple x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (TyList x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TyApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (TyVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TyCon x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TyParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TyInfix x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (TyKind x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData Boxed where
  rnf Boxed = ()
  rnf Unboxed = () 

instance NFData l => NFData (Kind l) where
  rnf (KindStar x1) = (rnf x1 `seq` ())
  rnf (KindBang x1) = (rnf x1 `seq` ())
  rnf (KindFn x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (KindParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (KindVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (TyVarBind l) where
  rnf (KindedVar x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (UnkindedVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Exp l) where
  rnf (Var x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IPVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Con x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Lit x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (InfixApp x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (App x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (NegApp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Lambda x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Let x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (If x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (Case x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Do x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (MDo x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Tuple x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TupleSection x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (List x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Paren x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (LeftSection x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RightSection x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RecConstr x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RecUpdate x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (EnumFrom x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (EnumFromTo x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (EnumFromThen x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (EnumFromThenTo x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (ListComp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (ParComp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (ExpTypeSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (VarQuote x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TypQuote x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (BracketExp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (SpliceExp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (QuasiQuote x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (XTag x1 x2 x3 x4 x5)
    = (rnf x1
     `seq`
       (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (XETag x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (XPcdata x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (XExpTag x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (XChildTag x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (CorePragma x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (SCCPragma x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (GenPragma x1 x2 x3 x4 x5)
    = (rnf x1
     `seq`
       (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (Proc x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (LeftArrApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RightArrApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (LeftArrHighApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RightArrHighApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Stmt l) where
  rnf (Generator x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Qualifier x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (LetStmt x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (RecStmt x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (QualStmt l) where
  rnf (QualStmt x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ThenTrans x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ThenBy x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (GroupBy x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (GroupUsing x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (GroupByUsing x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (FieldUpdate l) where
  rnf (FieldUpdate x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (FieldPun x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (FieldWildcard x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (Alt l) where
  rnf (Alt x1 x2 x3 x4) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ())))) 

instance NFData l => NFData (GuardedAlts l) where
  rnf (UnGuardedAlt x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (GuardedAlts x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (GuardedAlt l) where
  rnf (GuardedAlt x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (XAttr l) where
  rnf (XAttr x1 x2 x3) = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Pat l) where
  rnf (PVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PLit x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PNeg x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PNPlusK x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PInfixApp x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (PApp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PTuple x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PList x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PRec x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PAsPat x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PWildCard x1) = (rnf x1 `seq` ())
  rnf (PIrrPat x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PatTypeSig x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PViewPat x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PRPat x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PXTag x1 x2 x3 x4 x5)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` ())))))
  rnf (PXETag x1 x2 x3 x4)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` ()))))
  rnf (PXPcdata x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PXPatTag x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PXRPats x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PExplTypeArg x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PQuasiQuote x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PBangPat x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (PatField l) where
  rnf (PFieldPat x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PFieldPun x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PFieldWildcard x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (PXAttr l) where
  rnf (PXAttr x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (RPat l) where
  rnf (RPOp x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RPEither x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RPSeq x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (RPGuard x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RPCAs x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RPAs x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (RPParen x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (RPPat x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (RPatOp l) where
  rnf (RPStar x1) = (rnf x1 `seq` ())
  rnf (RPStarG x1) = (rnf x1 `seq` ())
  rnf (RPPlus x1) = (rnf x1 `seq` ())
  rnf (RPPlusG x1) = (rnf x1 `seq` ())
  rnf (RPOpt x1) = (rnf x1 `seq` ())
  rnf (RPOptG x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (Literal l) where
  rnf (Char x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (String x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Int x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Frac x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimInt x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimWord x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimFloat x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimDouble x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimChar x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (PrimString x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (ModuleName l) where
  rnf (ModuleName x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (QName l) where
  rnf (Qual x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (UnQual x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Special x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Name l) where
  rnf (Ident x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (Symbol x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (QOp l) where
  rnf (QVarOp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (QConOp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Op l) where
  rnf (VarOp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ConOp x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (SpecialCon l) where
  rnf (UnitCon x1) = (rnf x1 `seq` ())
  rnf (ListCon x1) = (rnf x1 `seq` ())
  rnf (FunCon x1) = (rnf x1 `seq` ())
  rnf (TupleCon x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (Cons x1) = (rnf x1 `seq` ())
  rnf (UnboxedSingleCon x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (CName l) where
  rnf (VarName x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ConName x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (IPName l) where
  rnf (IPDup x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (IPLin x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (XName l) where
  rnf (XName x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (XDomName x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Bracket l) where
  rnf (ExpBracket x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (PatBracket x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TypeBracket x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (DeclBracket x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Splice l) where
  rnf (IdSplice x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ParenSplice x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Safety l) where
  rnf (PlayRisky x1) = (rnf x1 `seq` ())
  rnf (PlaySafe x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (CallConv l) where
  rnf (StdCall x1) = (rnf x1 `seq` ())
  rnf (CCall x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (ModulePragma l) where
  rnf (LanguagePragma x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (OptionsPragma x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (AnnModulePragma x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData Tool where
  rnf GHC = ()
  rnf HUGS = ()
  rnf NHC98 = ()
  rnf YHC = ()
  rnf HADDOCK = ()
  rnf (UnknownTool x1) = (rnf x1 `seq` ()) 

instance NFData l => NFData (Rule l) where
  rnf (Rule x1 x2 x3 x4 x5 x6)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` (rnf x4 `seq` (rnf x5 `seq` (rnf x6 `seq` ())))))) 

instance NFData l => NFData (RuleVar l) where
  rnf (RuleVar x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (TypedRuleVar x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ()))) 

instance NFData l => NFData (Activation l) where
  rnf (ActiveFrom x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ()))
  rnf (ActiveUntil x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 

instance NFData l => NFData (Annotation l) where
  rnf (Ann x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (TypeAnn x1 x2 x3)
    = (rnf x1 `seq` (rnf x2 `seq` (rnf x3 `seq` ())))
  rnf (ModuleAnn x1 x2) = (rnf x1 `seq` (rnf x2 `seq` ())) 



{-
-- derive instances for Doc
$( derive makeSerialize ''Doc )
$( derive makeNFData ''Doc )

-- derive NFData instances for haskell-src-exts
$( derive makeNFData ''Module )
$( derive makeNFData ''ModuleHead )
$( derive makeNFData ''WarningText )
$( derive makeNFData ''ExportSpecList )
$( derive makeNFData ''ExportSpec )
$( derive makeNFData ''ImportDecl )
$( derive makeNFData ''ImportSpecList )
$( derive makeNFData ''ImportSpec )
$( derive makeNFData ''Assoc )
$( derive makeNFData ''Decl )
$( derive makeNFData ''DeclHead )
$( derive makeNFData ''InstHead )
$( derive makeNFData ''Binds )
$( derive makeNFData ''IPBind )
$( derive makeNFData ''ClassDecl )
$( derive makeNFData ''InstDecl )
$( derive makeNFData ''Deriving )
$( derive makeNFData ''DataOrNew )
$( derive makeNFData ''ConDecl )
$( derive makeNFData ''FieldDecl )
$( derive makeNFData ''QualConDecl )
$( derive makeNFData ''GadtDecl )
$( derive makeNFData ''BangType )
$( derive makeNFData ''Match )
$( derive makeNFData ''Rhs )
$( derive makeNFData ''GuardedRhs )
$( derive makeNFData ''Context )
$( derive makeNFData ''FunDep )
$( derive makeNFData ''Asst )
$( derive makeNFData ''Type )
$( derive makeNFData ''Boxed )
$( derive makeNFData ''Kind )
$( derive makeNFData ''TyVarBind )
$( derive makeNFData ''Exp )
$( derive makeNFData ''Stmt )
$( derive makeNFData ''QualStmt )
$( derive makeNFData ''FieldUpdate )
$( derive makeNFData ''Alt )
$( derive makeNFData ''GuardedAlts )
$( derive makeNFData ''GuardedAlt )
$( derive makeNFData ''XAttr )
$( derive makeNFData ''Pat )
$( derive makeNFData ''PatField )
$( derive makeNFData ''PXAttr )
$( derive makeNFData ''RPat )
$( derive makeNFData ''RPatOp )
$( derive makeNFData ''Literal )
$( derive makeNFData ''ModuleName )
$( derive makeNFData ''QName )
$( derive makeNFData ''Name )
$( derive makeNFData ''QOp )
$( derive makeNFData ''Op )
$( derive makeNFData ''SpecialCon )
$( derive makeNFData ''CName )
$( derive makeNFData ''IPName )
$( derive makeNFData ''XName )
$( derive makeNFData ''Bracket )
$( derive makeNFData ''Splice )
$( derive makeNFData ''Safety )
$( derive makeNFData ''CallConv )
$( derive makeNFData ''ModulePragma )
$( derive makeNFData ''Tool )
$( derive makeNFData ''Rule )
$( derive makeNFData ''RuleVar )
$( derive makeNFData ''Activation )
$( derive makeNFData ''Annotation )
-}

