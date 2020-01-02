module Exercises (findBonding) where

import Data.List


-- Exercise A5
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding f xs = if (2* (length value) == length(xs)) then Just ([(x,y) | (y,x) <- value] ++ value) else Nothing where
    value = helper f xs xs

helper :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> [(a,a)]
helper _ (x:[]) _ = []
helper _ _ (x:[]) = []
helper _ _ [] = []
helper _ [] _ = []


helper predicate (x:y:xs) wholelist = if (predicate x y && 2 *(length remainingbindings) + 2 == length wholelist) then [(x,y)] ++ remainingbindings else helper predicate (x:xs) wholelist where
    remainingbindings = helper predicate wholesublist wholesublist where
        wholesublist = [b | b <- wholelist, (b /= x && b /= y)]