{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Scion.PersistentBrowser.Parser.Documentable where

import Scion.PersistentBrowser.Types
import qualified Language.Haskell.Exts.Syntax as S
import qualified Language.Haskell.Exts.Annotated.Syntax as A

class Documentable a b | a -> b where
  document :: Doc -> a -> (Documented b)

instance Documentable S.Type A.Type where
  document d (S.TyForall b c t) = A.TyForall d (fmap (fmap (document d)) b)
                                               (Just (document d c))
                                               (document d t)
  document d (S.TyFun t1 t2)    = A.TyFun d (document d t1)
                                            (document d t2)
  document d (S.TyTuple b t)    = A.TyTuple d (documentBoxed d b)
                                              (fmap (document d) t)
  document d (S.TyList t)       = A.TyList d (document d t)
  document d (S.TyApp t1 t2)    = A.TyApp d (document d t1)
                                            (document d t2)
  document d (S.TyVar n)        = A.TyVar d (document d n)
  document d (S.TyCon c)        = A.TyCon d (document d c)
  document d (S.TyParen t)      = A.TyParen d (document d t)
  document d (S.TyInfix l n r)  = A.TyInfix d (document d l)
                                              (document d n)
                                              (document d r)
  document d (S.TyKind t k)     = A.TyKind d (document d t)
                                             (document d k)

instance Documentable S.TyVarBind A.TyVarBind where
  document d (S.KindedVar n k) = A.KindedVar d (document d n)
                                               (document d k)
  document d (S.UnkindedVar n) = A.UnkindedVar d (document d n)

instance Documentable S.Context A.Context where
  document d [a] = A.CxSingle d (document d a)
  document d v   = A.CxTuple d (fmap (document d) v)

instance Documentable S.Asst A.Asst where
  document d (S.ClassA q t)   = A.ClassA d (document d q)
                                           (fmap (document d) t)
  document d (S.InfixA l q r) = A.InfixA d (document d l)
                                           (document d q)
                                           (document d r)
  document d (S.IParam n t)   = A.IParam d (document d n)
                                           (document d t)
  document d (S.EqualP t1 t2) = A.EqualP d (document d t1)
                                           (document d t2)

instance Documentable S.Name A.Name where
  document d (S.Ident s)  = A.Ident d s
  document d (S.Symbol s) = A.Symbol d s

instance Documentable S.QName A.QName where
  document d (S.Qual mn n) = A.Qual d (document d mn)
                                      (document d n)
  document d (S.UnQual n)  = A.UnQual d (document d n)
  document d (S.Special c) = A.Special d (document d c)

instance Documentable S.ModuleName A.ModuleName where
  document d (S.ModuleName n) = A.ModuleName d n

instance Documentable S.IPName A.IPName where
  document d (S.IPDup s) = A.IPDup d s
  document d (S.IPLin s) = A.IPLin d s

instance Documentable S.SpecialCon A.SpecialCon where
  document d S.UnitCon          = A.UnitCon d
  document d S.ListCon          = A.ListCon d
  document d S.FunCon           = A.FunCon d
  document d (S.TupleCon b n)   = A.TupleCon d (documentBoxed d b) n
  document d S.Cons             = A.Cons d
  document d S.UnboxedSingleCon = A.UnboxedSingleCon d

instance Documentable S.Kind A.Kind where
  document d S.KindStar       = A.KindStar d
  document d S.KindBang       = A.KindBang d
  document d (S.KindFn k1 k2) = A.KindFn d (document d k1)
                                           (document d k2)
  document d (S.KindParen k)  = A.KindParen d (document d k)
  document d (S.KindVar n)    = A.KindVar d (document d n)

documentBoxed :: Doc -> S.Boxed -> A.Boxed
documentBoxed _ S.Boxed   = A.Boxed
documentBoxed _ S.Unboxed = A.Unboxed

