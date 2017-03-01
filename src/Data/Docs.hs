module Data.Docs
  ( Documentation(..)
  , Field(..)
  , Docs(..)
  , genDocs
  , genDocsWith
  , FieldModifier
  , genDocsEnum
  , genValues
  , genFields
  , Sample(..)
  , markdown
  ) where

import Data.Docs.Docs
import Data.Docs.Sample
import Data.Docs.ToMarkdown (markdown)
