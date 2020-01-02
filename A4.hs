module Exercises (neighbours) where

import Data.List
import Data.Ord

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | k < 0 = error "k cannot be negative"
                    | length xs == 1 = xs
                    | length xs < k = xs
                    | otherwise = take k $ sortBy (comparing (d p)) xs



             