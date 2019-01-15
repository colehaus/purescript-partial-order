module Test.Main where

import Prelude (Unit, ($), (&&), (=<<), (<<<), (/=), discard)
import Prelude as Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Ord.Partial
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Test.QuickCheck (Result(..), (===), (<=?))
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Mathematical properties" do
    it "reflexive" $
      quickCheck \(a :: Maybe Int) -> a <=? a
    it "antisymmetric" $
      quickCheck \(a :: Maybe Int) b -> if a <= b && b <= a then a === b else Success
    it "transitive" $
      quickCheck \(a :: Maybe Int) b c ->
        let minM = min a =<< (min b c)
            maxM = max a =<< (max b c)
            midM = Array.head <<< Array.filter (\x -> Just x /= minM && Just x /= maxM) $ [a, b, c]
        in case Tuple minM (Tuple midM maxM) of
          Tuple (Just min') (Tuple (Just mid) (Just max')) ->
            if min' <= mid && mid <= max'
            then min' <=? max'
            else Success
          _ -> Success
  describe "Compared to total order" do
    it "<" $
      quickCheck \(a :: Int) b -> (a < b) === (a Prelude.< b)
    it ">" $
      quickCheck \(a :: Int) b -> (a > b) === (a Prelude.> b)
    it "<=" $
      quickCheck \(a :: Int) b -> (a <= b) === (a Prelude.<= b)
    it ">=" $
      quickCheck \(a :: Int) b -> (a >= b) === (a Prelude.>= b)
    it "min" $
      quickCheck \(a :: Int) b -> min a b === Just (Prelude.min a b)
    it "max" $
      quickCheck \(a :: Int) b -> max a b === Just (Prelude.max a b)
    it "clamp" $
      quickCheck \(a :: Int) b c -> clamp a b c === Just (Prelude.clamp a b c)
    it "between" $
      quickCheck \(a :: Int) b c -> between a b c === Prelude.between a b c
