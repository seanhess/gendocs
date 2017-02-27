Generate interface documentation for your types

Example
-------

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    module Example where

    import Data.Aeson (ToJSON)
    import Data.Proxy (Proxy(..))
    import Data.Docs (markdown, Docs(..), Sample(..), genDocs, genDocsEnum)
    import Data.Text (Text)
    import qualified Data.Text as Text
    import GHC.Generics (Generic)

    data Person = Person
        { name :: Text
        , age :: Age
        , hobbies :: [Hobby]
        } deriving (Show, Eq, Generic)
    instance ToJSON Person
    instance Docs Person
    instance Sample Person where
        sample _ = Person
          { name = "Bob"
          , age = sample (Proxy :: Proxy Age)
          , hobbies = [Haskell, Friends]
          }

    newtype Age = Age Int
        deriving (Show, Eq, Generic)
    instance ToJSON Age
    instance Docs Age where
        docs = genDocs "Age in years"
    instance Sample Age where
        sample _ = Age 31
        samples _ = [Age 31, Age 24]

    data Hobby = Haskell | Friends | Movies
        deriving (Show, Eq, Generic, Enum, Bounded)
    instance ToJSON Hobby
    instance Docs Hobby where
        docs = genDocsEnum "Hobby"
    instance Sample Hobby where
        sample _ = Haskell

    doc :: IO ()
    doc = do
        putStrLn $ Text.unpack json

    json :: Text
    json = Text.intercalate "\n\n\n" $
      [ markdown (Proxy :: Proxy Person)
      , markdown (Proxy :: Proxy Age)
      , markdown (Proxy :: Proxy Hobby)
      ]

This generates the following output

    Person
    ---------------------

    | Field   | Type              |
    |---------|-------------------|
    | name    | [Text](#text)     |
    | age     | [Age](#age)       |
    | hobbies | [[Hobby]](#hobby) |

    ```
    {
        "age": 31,
        "hobbies": [
            "Haskell",
            "Friends"
        ],
        "name": "Bob"
    }
    ```


    Age
    ---------------------

    Age in years

    ```
    31
    ```


    Hobby
    ---------------------

    | Values    |
    |-----------|
    | "Haskell" |
    | "Friends" |
    | "Movies"  |

    ```
    "Haskell"
    ```
