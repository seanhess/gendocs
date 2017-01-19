{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Docs.TypeName
  ( typeName
  , GTypeName(gtypename)
  ) where

import Data.Data (Proxy(..))
import GHC.Generics


-- | Get the typename for anything that has a simple one. https://gist.github.com/nh2/1a03b7873dbed348ef64fe536028776d

typeName :: forall a. (Generic a, GTypeName (Rep a)) => Proxy a -> String
typeName _proxy = gtypename (from (undefined :: a))

class GTypeName f where
  gtypename :: f a -> String

instance (Datatype c) => GTypeName (M1 i c f) where
  gtypename m = datatypeName m
