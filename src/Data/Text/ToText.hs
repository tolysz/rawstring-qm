
{-# Language TypeSynonymInstances
           , FlexibleInstances #-}

module Data.Text.ToText where

import Prelude
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)

class ToText a where
    toText :: a -> Text

instance ToText Int where
    toText = toStrict . toLazyText . decimal
instance ToText Integer where
    toText = toStrict . toLazyText . decimal

instance ToText Float where
    toText = toStrict . toLazyText . realFloat

instance ToText Text where
    toText = id

instance ToText String where
    toText = pack
