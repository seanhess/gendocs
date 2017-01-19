{-# LANGUAGE OverloadedStrings #-}
module Data.Docs.ToMarkdown where

import Data.Aeson (ToJSON, toJSON, Value, encode)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(..))
import Data.Char (isAlphaNum)
import Data.Data (Proxy(..))
import Data.Docs.Docs (Documentation(..), Field(..), Docs(..))
import Data.Docs.Sample (Sample(..))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Safe (maximumDef, headMay)

-- | Generate markdown documentation
markdown :: (ToJSON a, Docs a, Sample a) => Proxy a -> Text
markdown p =
  let d = docs p
      s = sample p
      vs =  allValues p
  in Text.intercalate "\n\n" $ map (Text.intercalate "\n") $ filter (not . List.null)
  [ header d
  , desc (description d)
  , fieldTable (fields d)
  , valuesTable (map toJSON vs)
  , example s
  ]

desc :: Text -> [Text]
desc "" = []
desc d = [d]

header :: Documentation -> [Text]
header d =
  [ typeName d
  , "---------------------"
  ]

example :: ToJSON a => a -> [Text]
example a =
  [ "```"
  , Text.decodeUtf8 $ BSL.toStrict $ encodePretty' conf a
  , "```"
  ]
  where
    conf = defConfig { confCompare = compare }


type URL = Text

link :: Text -> URL -> Text
link t u =
    "[" <> t <> "](" <> u <> ")"

anchor :: Text -> Text -> Text
anchor t n = link t $ "#" <> (Text.filter isAlphaNum $ Text.toLower n)


fieldTable :: [Field] -> [Text]
fieldTable [] = []
fieldTable fs =
  table ["Field", "Type", ""] (map row fs)
  where
    row f =
      [ fieldName f
      , anchor (fieldType f) (fieldType f)
      , opInfo f
      ]
    opInfo f =
      if isRequired f
        then ""
        else "(optional)"


valuesTable :: [Value] -> [Text]
valuesTable [] = []
valuesTable vs =
  table ["Values"] (map row vs)
  where
    row t = [ Text.decodeUtf8 $ BSL.toStrict $ encode t ]


-- | Tables

type CellWidth = Int


cellWidth :: [Text] -> Int
cellWidth ts = maximumDef 0 (map Text.length ts)


table :: [Text] -> [[Text]] -> [Text]
table hs rs =
  let jr = List.transpose $ map justifyColumn $ filter nonEmptyCol $ List.transpose (hs : rs) :: [[Text]]
      hs' = fromMaybe [] $ headMay jr :: [Text]
      rs' = drop 1 jr
  in mconcat [ tableHeader hs', map tableRow rs' ]
  where
    nonEmptyCol = not . all Text.null . List.drop 1

      -- calculate the column width for a column


justifyColumn :: [Text] -> [Text]
justifyColumn ts =
  let w = maximumDef 0 (map Text.length ts)
  in map (Text.justifyLeft w ' ') ts



tableHeader :: [Text] -> [Text]
tableHeader ts =
  [tableRow ts, sepRow ts]


tableRow :: [Text] -> Text
tableRow ts =
  mconcat ["| ", Text.intercalate " | " ts, " |"]
  where


sepRow :: [Text] -> Text
sepRow ts =
  mconcat ["|-", Text.intercalate "-|-" (map sepCell ts), "-|"]
  where
    sepCell t = Text.replicate (Text.length t) "-"


