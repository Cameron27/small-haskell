module Common.Functions where

import Data.List

-- | @allDifferent xs@ returns `True` iff all the elements in `xs` are distinct.
allDifferent :: (Ord a, Eq a) => [a] -> Bool
allDifferent = comparePairwise . sort
  where
    comparePairwise [] = True
    comparePairwise [_] = True
    comparePairwise (x : y : xs)
      | x == y = False
      | otherwise = comparePairwise (y : xs)