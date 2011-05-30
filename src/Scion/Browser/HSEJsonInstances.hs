{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Scion.Browser.HSEJsonInstances where

import Data.Aeson
import qualified Data.Text as T
import Scion.Browser.HSEInstances
import Language.Haskell.Exts.Annotated.Syntax hiding (String)
import Language.Haskell.Exts.Pretty

instance ToJSON Doc where
  toJSON (NoDoc)   = Null
  toJSON (Doc txt) = String txt

instance ToJSON (Name l) where
  toJSON = String . T.pack . getNameString

instance ToJSON (QName l) where
  toJSON = String . T.pack . getQNameString

singleLinePrettyPrint :: Pretty a => a -> String
singleLinePrettyPrint = prettyPrintWithMode $ defaultMode { layout = PPNoLayout }

instance ToJSON (Kind l) where
  toJSON = String . T.pack . singleLinePrettyPrint

instance ToJSON (Type l) where
  toJSON = String . T.pack . singleLinePrettyPrint

instance ToJSON (Documented Module) where
  toJSON (Module doc (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = object [ T.pack "doc"  .= doc
                                                                                   , T.pack "name" .= name
                                                                                   ]
  toJSON _ = Null

instance ToJSON (Documented Decl) where
  toJSON (GDataDecl doc dOrM ctx hd kind decls _) = object [ T.pack "doc"     .= doc
                                                           , T.pack "type"    .= dOrM
                                                           , T.pack "context" .= ctx
                                                           , T.pack "head"    .= hd
                                                           , T.pack "kind"    .= kind
                                                           , T.pack "decls"   .= decls
                                                           ]
  toJSON (ClassDecl doc ctx hd fdeps _)           = object [ T.pack "doc"     .= doc
                                                           , T.pack "type"    .= T.pack "class"
                                                           , T.pack "context" .= ctx
                                                           , T.pack "head"    .= hd
                                                           , T.pack "fundeps" .= fdeps
                                                           ]
  toJSON (InstDecl doc ctx hd _)                  = object [ T.pack "doc"     .= doc
                                                           , T.pack "type"    .= T.pack "instance"
                                                           , T.pack "context" .= ctx
                                                           , T.pack "head"    .= hd
                                                           ]
  toJSON (TypeSig doc [name] ty)                  = object [ T.pack "doc"     .= doc
                                                           , T.pack "type"    .= T.pack "signature"
                                                           , T.pack "name"    .= name
                                                           , T.pack "type"    .= ty
                                                           ]
  toJSON (TypeDecl doc hd ty)                     = object [ T.pack "doc"     .= doc
                                                           , T.pack "type"    .= T.pack "type"
                                                           , T.pack "head"    .= hd
                                                           , T.pack "type"    .= ty
                                                           ]
  toJSON _ = Null

instance ToJSON (Context l) where
  toJSON (CxSingle _ a)  = toJSON [a]
  toJSON (CxTuple _ as)  = toJSON as
  toJSON (CxParen _ ctx) = toJSON ctx
  toJSON (CxEmpty _)     = toJSON ([] :: [Asst l])

instance ToJSON (Asst l) where
  toJSON = String . T.pack . singleLinePrettyPrint

instance ToJSON (DataOrNew l) where
  toJSON (DataType _) = String $ T.pack "data"
  toJSON (NewType  _) = String $ T.pack "newtype"

instance ToJSON (DeclHead l) where
  toJSON (DHead _ name vars) = object [ T.pack "name" .= name
                                      , T.pack "vars" .= vars
                                      ]
  toJSON _ = Null

instance ToJSON (TyVarBind l) where
  toJSON = String . T.pack . singleLinePrettyPrint

instance ToJSON (FunDep l) where
  toJSON = String . T.pack . singleLinePrettyPrint

instance ToJSON (InstHead l) where
  toJSON (IHead _ name vars) = object [ T.pack "name" .= name
                                      , T.pack "vars" .= vars
                                      ]
  toJSON _ = Null

instance ToJSON (Documented GadtDecl) where
  toJSON (GadtDecl _ name ty) = object [ T.pack "name" .= name
                                       , T.pack "type" .= ty
                                       ]

