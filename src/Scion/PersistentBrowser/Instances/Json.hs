{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
-- FlexibleInstances needed for GHC 7.2

module Scion.PersistentBrowser.Instances.Json where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Scion.PersistentBrowser.DbTypes

instance ToJSON DbPackage where
  toJSON (DbPackage name version doc) = object [ "id"  .= object [ "name"    .= T.pack name
                                                                 , "version" .= T.pack version
                                                                 ]
                                               , "doc" .= doc
                                               ]

instance ToJSON DbPackageIdentifier where
  toJSON (DbPackageIdentifier name version) = object [ "name"    .= T.pack name
                                                     , "version" .= T.pack version
                                                     ]

instance FromJSON DbPackageIdentifier where
  parseJSON (Object v) = DbPackageIdentifier <$> (T.unpack <$> v .: "name")
                                             <*> (T.unpack <$> v .: "version")
  parseJSON _          = mzero

instance ToJSON (DbModule) where
  toJSON (DbModule name doc _) = object [ "name" .= T.pack name
                                        , "doc"  .= doc
                                        ]

instance ToJSON DbCompleteDecl where
  toJSON (DbCompleteDecl (DbDecl DbData name doc kind _ _ _) context vars _ decls) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "data"
           , "context"   .= context
           , "head"      .= object [ "name" .= name
                                   , "vars" .= vars
                                   ]
           , "kind"      .= kind
           , "decls"     .= decls
           ]
  toJSON (DbCompleteDecl (DbDecl DbNewType name doc kind _ _ _) context vars _ decls) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "newtype"
           , "context"   .= context
           , "head"      .= object [ "name" .= name
                                   , "vars" .= vars
                                   ]
           , "kind"      .= kind
           , "decls"     .= decls
           ]
  toJSON (DbCompleteDecl (DbDecl DbClass name doc _ _ _ _) context vars fundeps _) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "class"
           , "context"   .= context
           , "head"      .= object [ "name" .= name
                                   , "vars" .= vars
                                   ]
           , "fundeps"   .= fundeps
           ]
  toJSON (DbCompleteDecl (DbDecl DbInstance name doc _ _ _ _) context vars _ _) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "instance"
           , "context"   .= context
           , "head"      .= object [ "name" .= name
                                   , "vars" .= vars
                                   ]
           ]
  toJSON (DbCompleteDecl (DbDecl DbSignature name doc _ signature _ _) _ _ _ _) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "signature"
           , "name"      .= [ name ]
           , "signature" .= signature
           ]
  toJSON (DbCompleteDecl (DbDecl DbType name doc _ _ equals _) _ vars _ _) =
    object [ "doc"       .= doc
           , "type"      .= T.pack "type"
           , "head"      .= object [ "name" .= name
                                   , "vars" .= vars
                                   ]
           , "equals"   .= equals
           ]

instance ToJSON DbContext where
  toJSON (DbContext shown _) = String $ T.pack shown

instance ToJSON DbTyVar where
  toJSON (DbTyVar name _) = String $ T.pack name

instance ToJSON DbFunDep where
  toJSON (DbFunDep name _) = String $ T.pack name

instance ToJSON DbConstructor where
  toJSON (DbConstructor name signature _) = object [ "name" .= name
                                                   , "type" .= signature
                                                   ]

