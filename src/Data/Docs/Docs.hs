{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Docs.Docs
  ( Documentation(..)
  , Field(..)
  , Docs(..)
  , genDocs
  , genDocsEnum
  , genValues
  , genFields
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Docs.TypeName (GTypeName)
import qualified Data.Docs.TypeName as TypeName
import Data.Docs.Selectors (Selectors(..))
import Data.Data (Proxy(..), TypeRep)
import GHC.Generics (Generic, Rep)


-- | Can generate documentation for type
class Docs a where
    docs :: Proxy a -> Documentation
    default docs :: (Generic a, GTypeName (Rep a), Selectors (Rep a)) => Proxy a -> Documentation
    docs = genDocs ""


type FieldName = Text
type TypeName = Text

-- | Documentation for a given type
data Documentation = Documentation
    { typeName :: TypeName
    , fields :: [Field]
    , description :: Text
    , enumeratedValues :: [Text]
    } deriving (Show, Eq)

-- | Documentation for a record field
data Field = Field
    { fieldName :: FieldName
    , fieldType :: TypeName
    , isRequired :: Bool
    } deriving (Show, Eq)


-- | Create documentation for a type with only a description
genDocs :: forall a. (Generic a, GTypeName (Rep a), Selectors (Rep a)) => Text -> Proxy a -> Documentation
genDocs d p = Documentation
    { typeName = pack $ TypeName.typeName p
    , description = d
    , fields = genFields p
    , enumeratedValues = []
    }

genDocsEnum :: forall a. (Generic a, GTypeName (Rep a), Selectors (Rep a), Bounded a, Enum a, Show a) => Text -> Proxy a -> Documentation
genDocsEnum d p =
    (genDocs d p)
      { enumeratedValues = genValues p }


genFields :: forall a. (Selectors (Rep a)) => Proxy a -> [Field]
genFields _ = map (uncurry toField . selectorToText) $ selectors (Proxy :: Proxy (Rep a))


genValues :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> [Text]
genValues _ = map (pack . show) ([minBound..] :: [a])


selectorToText :: (String, TypeRep) -> (Text, [Text])
selectorToText (s, t) = (pack s, T.words $ pack $ show t)


toField :: Text -> [Text] -> Field
toField s ("Maybe":t) = Field s (T.concat t) False
toField s t = Field s (T.concat t) True
