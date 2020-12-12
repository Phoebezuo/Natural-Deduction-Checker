module Util
    ( mapTraversal
    , getAllIndex
    ) where

import Prelude ((>>>), ($), (==), class Eq, bind, discard, pure)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Data.Array as A
import Control.MonadZero (guard)

fromIdentity :: forall a. Identity a -> a
fromIdentity (Identity x) = x

mapTraversal :: forall a b x y. ((a -> Identity b) -> x -> Identity y) ->
    (a -> b) -> x -> y
mapTraversal t f = t (f >>> Identity) >>> fromIdentity

getAllIndex :: forall a. Eq a => a -> Array a -> Array Int
getAllIndex x a = do
    Tuple i v <- A.mapWithIndex Tuple a
    guard $ v == x
    pure i
