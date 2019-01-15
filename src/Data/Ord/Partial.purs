module Data.Ord.Partial where

import Prelude (Ordering(..), ($), (&&), (<=<))
import Prelude as Prelude

import Data.Maybe (Maybe(..))

class (Prelude.Eq a) <= PartialOrd a where
  compare :: a -> a -> Maybe Ordering

instance maybePartialOrd :: (Prelude.Ord a) => PartialOrd (Maybe a) where
  compare Nothing _ = Nothing
  compare _ Nothing = Nothing
  compare (Just a) (Just b) = Just $ Prelude.compare a b
else
instance ordPartialOrd :: (Prelude.Ord a) => PartialOrd a where
  compare a b = Just $ Prelude.compare a b

lessThan :: forall a. PartialOrd a => a -> a -> Boolean
lessThan a b =
  case a `compare` b of
    Just LT -> true
    _ -> false

greaterThan :: forall a. PartialOrd a => a -> a -> Boolean
greaterThan a b =
  case a `compare` b of
    Just GT -> true
    _ -> false

lessThanOrEq :: forall a. PartialOrd a => a -> a -> Boolean
lessThanOrEq a b =
  case a `compare` b of
    Just LT -> true
    Just EQ -> true
    Just GT -> false
    Nothing -> false

greaterThanOrEq :: forall a. PartialOrd a => a -> a -> Boolean
greaterThanOrEq a b =
  case a `compare` b of
    Just GT -> true
    Just EQ -> true
    Just LT -> false
    Nothing -> false

infixl 4 lessThan as <
infixl 4 lessThanOrEq as <=
infixl 4 greaterThan as >
infixl 4 greaterThanOrEq as >=

comparing :: forall a b. PartialOrd b => (a -> b) -> (a -> a -> Maybe Ordering)
comparing f a b = compare (f a) (f b)

min :: forall a. PartialOrd a => a -> a -> Maybe a
min a b =
  case a `compare` b of
    Just LT -> Just a
    Just EQ -> Just a
    Just GT -> Just b
    Nothing -> Nothing

max :: forall a. PartialOrd a => a -> a -> Maybe a
max a b =
  case a `compare` b of
    Just LT -> Just b
    Just EQ -> Just a
    Just GT -> Just a
    Nothing -> Nothing

clamp :: forall a. PartialOrd a => a -> a -> a -> Maybe a
clamp low hi a = min hi <=< max low $ a

between :: forall a. PartialOrd a => a -> a -> a -> Boolean
between low hi a = low <= a && a <= hi
