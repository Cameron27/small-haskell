module TypeChecker.Helper.Functions where

import Data.List

allDifferent :: (Ord a, Eq a) => [a] -> Bool
allDifferent = comparePairwise . sort

comparePairwise :: Eq a => [a] -> Bool
comparePairwise [] = True
comparePairwise [_] = True
comparePairwise (x : y : xs)
  | x == y = False
  | otherwise = comparePairwise (y : xs)