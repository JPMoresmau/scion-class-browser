{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Scion.Browser.Instances.Json where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Version
import qualified Data.Text as T
import Distribution.Package hiding (Package)
import Scion.Browser.Types
import Language.Haskell.Exts.Annotated.Syntax hiding (String)
import Language.Haskell.Exts.Pretty
import Text.ParserCombinators.ReadP

instance ToJSON (Documented Package) where
  toJSON (Package doc pid _) = object [ T.pack "id"  .= pid
                                      , T.pack "doc" .= doc
                                      ]

instance ToJSON PackageIdentifier where
  toJSON (PackageIdentifier (PackageName name) version) = object [ T.pack "name"    .= name
                                                                 , T.pack "version" .= version
                                                                 ]
instance FromJSON PackageIdentifier where
  parseJSON (Object v) = PackageIdentifier <$> (PackageName <$> v .: T.pack "name")
                                           <*> v .: T.pack "version"
  parseJSON _          = mzero

instance ToJSON Version where
  toJSON = String . T.pack . showVersion

instance FromJSON Version where
  parseJSON version = (fst . last . readP_to_S parseVersion . T.unpack) <$> parseJSON version

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

maybeEmptyContext :: Maybe (Documented Context) -> Documented Context
maybeEmptyContext Nothing    = CxEmpty NoDoc
maybeEmptyContext (Just ctx) = ctx

instance ToJSON (Documented Decl) where
  toJSON (GDataDecl doc dOrM ctx hd kind decls _) = object [ T.pack "doc"      .= doc
                                                           , T.pack "type"     .= dOrM
                                                           , T.pack "context"  .= maybeEmptyContext ctx
                                                           , T.pack "head"     .= hd
                                                           , T.pack "kind"     .= kind
                                                           , T.pack "decls"    .= decls
                                                           ]
  toJSON (ClassDecl doc ctx hd fdeps _)           = object [ T.pack "doc"      .= doc
                                                           , T.pack "type"     .= T.pack "class"
                                                           , T.pack "context"  .= maybeEmptyContext ctx
                                                           , T.pack "head"     .= hd
                                                           , T.pack "fundeps"  .= fdeps
                                                           ]
  toJSON (InstDecl doc ctx hd _)                  = object [ T.pack "doc"      .= doc
                                                           , T.pack "type"     .= T.pack "instance"
                                                           , T.pack "context"  .= maybeEmptyContext ctx
                                                           , T.pack "head"     .= hd
                                                           ]
  toJSON (TypeSig doc names ty)                  = object [ T.pack "doc"      .= doc
                                                           , T.pack "type"     .= T.pack "signature"
                                                           , T.pack "name"     .= names
                                                           , T.pack "signature" .= ty
                                                           ]
  toJSON (TypeDecl doc hd ty)                     = object [ T.pack "doc"      .= doc
                                                           , T.pack "type"     .= T.pack "type"
                                                           , T.pack "head"     .= hd
                                                           , T.pack "equals"   .= ty
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

