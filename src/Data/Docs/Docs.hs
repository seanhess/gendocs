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
  , genDocsWith
  , genDocsEnum
  , genValues
  , genFields
  , FieldModifier
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
    , fields = genFields id p
    , enumeratedValues = []
    }


genDocsWith :: forall a. (Generic a, GTypeName (Rep a), Selectors (Rep a)) => FieldModifier -> Text -> Proxy a -> Documentation
genDocsWith fieldModifier d p = Documentation
    { typeName = pack $ TypeName.typeName p
    , description = d
    , fields = genFields fieldModifier p
    , enumeratedValues = []
    }


genDocsEnum :: forall a. (Generic a, GTypeName (Rep a), Selectors (Rep a), Bounded a, Enum a, Show a) => Text -> Proxy a -> Documentation
genDocsEnum d p =
    (genDocs d p)
      { enumeratedValues = genValues p }


type FieldModifier = String -> String


genFields :: forall a. (Selectors (Rep a)) => FieldModifier -> Proxy a -> [Field]
genFields fieldModifier _ = map (uncurry toField . selectorToText fieldModifier) $ selectors (Proxy :: Proxy (Rep a))


genValues :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> [Text]
genValues _ = map (pack . show) ([minBound..] :: [a])


selectorToText :: FieldModifier -> (String, TypeRep) -> (String, [String])
selectorToText fieldModifier (s, t) = (fieldModifier s, words $ show t)


toField :: String -> [String] -> Field
toField s ("Maybe":t) = field s (concat t) False
toField s t = field s (concat t) True

field :: String -> String -> Bool -> Field
field n t o = Field (T.pack n) (T.pack t) o
