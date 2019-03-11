module Utils where

import Prelude

import Data.Array (intercalate)
import Data.Foldable (class Foldable)


unwords :: forall f s. Show s => Functor f => Foldable f => f s -> String
unwords = map show >>> intercalate " "
