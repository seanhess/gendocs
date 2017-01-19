{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Docs.Selectors where

import Data.Data (Proxy(..), TypeRep, Typeable, typeOf)
import GHC.Generics

-- data Record = Record { recordId :: Int32, recordName :: Text }
--   deriving (Generic)
-- selectors (Proxy :: Proxy (Rep Record))

class Selectors rep where
  selectors :: Proxy rep -> [(String, TypeRep)]

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

-- instance Selectors f => Selectors (C1 C x f) where
--   selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    let sel = selName (undefined :: M1 S s (K1 R t) ())
        typ = typeOf (undefined :: t)
    in if not (null sel)
      then [( sel , typ )]
      else []

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

-- | We don't really want to deal with sum types
instance (Selectors a, Selectors b) => Selectors (a :+: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []
