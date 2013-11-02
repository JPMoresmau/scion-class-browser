{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Scion.PersistentHoogle.Instances.Json where

import Control.Applicative
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import Scion.PersistentBrowser ()
import Scion.PersistentHoogle.Types

-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
--     toJSON (a,b,c,d) = toJSON [toJSON a, toJSON b, toJSON c, toJSON d]
--     {-# INLINE toJSON #-}

instance ToJSON (Result) where
  toJSON (RPackage pids)      = object [ "type"    .= T.pack "package"
                                       , "results" .= pids
                                       ]
  toJSON (RModule mds)        = object [ "type"    .= T.pack "module"
                                       , "results" .= mds
                                       ]
  toJSON (RDeclaration decls) = object [ "type"    .= T.pack "declaration"
                                       , "results" .= decls
                                       ]
  toJSON (RConstructor decls) = object [ "type"    .= T.pack "constructor"
                                       , "results" .= decls
                                       ]
  toJSON (RKeyword kw)        = object [ "type"    .= T.pack "keyword"
                                       , "name"    .= kw
                                       ]
  toJSON (RWarning w)         = object [ "type"    .= T.pack "warning"
                                       , "name"    .= w
                                       ]

instance FromJSON (Query) where
  parseJSON q = Query <$> parseJSON q

