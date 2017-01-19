-- a list type is annoying

module Data.Docs.Sample where

import Data.Proxy (Proxy)

class Sample a where
    sample :: Proxy a -> a

    samples :: Proxy a -> [a]
    samples p = [sample p]

    -- | If the values are bounded (an enum), list all of them
    allValues :: Proxy a -> [a]
    allValues _ = []
