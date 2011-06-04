module Scion.Hoogle.Instances.Json where

import Control.Applicative
import Data.Aeson hiding (Result)
import qualified Data.Text as T
import Scion.Hoogle.Types

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
    toJSON (a,b,c,d) = toJSON [toJSON a, toJSON b, toJSON c, toJSON d]
    {-# INLINE toJSON #-}

instance ToJSON (Result) where
  toJSON (RPackage pids)      = object [ T.pack "type"         .= "package"
                                       , T.pack "packages"     .= pids
                                       ]
  toJSON (RModule mods)       = object [ T.pack "type"         .= "module"
                                       , T.pack "modules"      .= mods
                                       ]
  toJSON (RDeclaration decls) = object [ T.pack "type"         .= "declaration"
                                       , T.pack "declarations" .= decls
                                       ]
  toJSON (RConstructor decls) = object [ T.pack "type"         .= "constructor"
                                       , T.pack "constructors" .= decls
                                       ]

instance FromJSON (Query) where
  parseJSON q = Query <$> parseJSON q

