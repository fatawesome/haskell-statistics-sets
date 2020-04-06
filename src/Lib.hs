{-# OPTIONS_GHC -Wall #-}

module Lib
  ( someFunc
  , mean
  , Mean
  , Variance
  , variance
  , Interval
  , IntervalSet
  , fromIntervalSet
  , unsafeIntervalSet
  , union
  , intervalSet
  ) where

import           Data.List     (sortBy)

import           Data.Function (on)

-- / Welford's mean
naiveMean :: Fractional a => [a] -> a
naiveMean xs = sum xs / fromIntegral (length xs)

data Mean a =
  Mean Int a
  deriving (Show)

instance Fractional a => Semigroup (Mean a) where
  Mean 0 _ <> Mean j y = Mean j y
  Mean i x <> Mean 0 _ = Mean i x
  Mean i x <> Mean j y = Mean (i + j) (x + ((y - x) * fromIntegral j / (fromIntegral i + fromIntegral j)))

instance Fractional a => Monoid (Mean a) where
  mempty = Mean 0 0

mean :: Fractional a => [a] -> Mean a
mean = mconcat . map (Mean 1)

-- / Chan's variance
-- / Intermediate results for
-- count, mean and variance.
data Variance a =
  Variance Int a a
  deriving (Show)

instance Fractional a => Semigroup (Variance a) where
  Variance 0 _ _ <> Variance cB meanB varB = Variance cB meanB varB
  Variance cA meanA varA <> Variance 0 _ _ = Variance cA meanA varA
  Variance cA meanA varA <> Variance cB meanB varB = Variance newCount newMean newVar
    where
      meanDelta = meanB - meanA
      countA = fromIntegral cA
      countB = fromIntegral cB
      newCount = cA + cB
      newMean = meanA + (meanDelta * countB / fromIntegral newCount)
      newVar =
        (varA * (countA - 1) + varB * (countB - 1) + meanDelta ^ 2 * countA * countB / fromIntegral newCount) /
        fromIntegral (newCount - 1)

instance Fractional a => Monoid (Variance a) where
  mempty = Variance 0 0 0

variance :: Fractional a => [a] -> Variance a
variance = mconcat . map (\x -> Variance 1 x 0)

-- / Interval sets
--
-- / A (closed) interval for values of type t.
--
-- Values of this type should always be valid.
--
-- from <= to
--
data Interval t =
  Interval
    { from :: t -- ^ Start of an interval
    , to   :: t -- ^ End of an interval
    }
  deriving (Show)

-- / A set of non-overlapping intervals.
--
-- The invariants for this representation are:
--
-- * the intervals are sorted according to from;
-- * the intervals do not overlap (even at a single point).
newtype IntervalSet t =
  IntervalSet [Interval t]
  deriving (Show)

-- / Convert an interval set into a list of pairs
-- representing each interval.
fromIntervalSet :: IntervalSet t -> [(t, t)]
fromIntervalSet (IntervalSet l) = map (\(Interval f t) -> (f, t)) l

-- / Unsafely construct an interval set from a list of pairs.
-- Input list should be ordered by the first component
-- and intervals, represented by (t, t) should not overlap.
unsafeIntervalSet :: [(t, t)] -> IntervalSet t
unsafeIntervalSet = IntervalSet . map (\pair -> Interval {from = fst pair, to = snd pair})

setToList :: IntervalSet t -> [Interval t]
setToList (IntervalSet l) = l

-- / An invariant-preserving union of two interval sets
-- хоспоть исус я голову сломал пока писал это в 12 часов ночи
union :: Ord t => IntervalSet t -> IntervalSet t -> IntervalSet t
union (IntervalSet s1) (IntervalSet []) = IntervalSet s1
union (IntervalSet []) (IntervalSet s2) = IntervalSet s2
union (IntervalSet s1) (IntervalSet s2)
  | to x < from y = IntervalSet (x : y : newTail)
  | from x > to y = IntervalSet (y : x : newTail)
  | otherwise = IntervalSet (newInterval : newTail)
  where
    x:xs = s1
    y:ys = s2
    start = min (from x) (from y)
    end = max (to x) (to y)
    newInterval = Interval start end
    newTail = setToList (IntervalSet xs `union` IntervalSet ys)

instance Ord t => Semigroup (IntervalSet t) where
  s1 <> s2 = s1 `union` s2

instance Ord t => Monoid (IntervalSet t) where
  mempty = IntervalSet []

orderingInPair :: Ord t => (t, t) -> (t, t)
orderingInPair (a, b)
  | a > b = (b, a)
  | otherwise = (a, b)

-- / Construct an interval set from an arbitrary list of intervals.
intervalSet :: Ord t => [(t, t)] -> IntervalSet t
intervalSet =
  IntervalSet .
  map (\pair -> Interval {from = fst pair, to = snd pair}) . (sortBy (compare `on` fst) . map orderingInPair)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
