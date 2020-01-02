module Exercises (approxSqrt) where

-- Exercise A2
approxSqrt :: Double -> Double -> Double
approxSqrt d eps | eps < 0 = error "epsilon cannot be negative"
                 | eps == 0 = error "epsilon cannot equal 0"
                 | d < 0 = error "d cannot be negative"
                 | otherwise = babylon d eps 1.0

babylon :: Double -> Double -> Double -> Double
babylon d epsilon val | (((val + d / val) / 2) - (sqrt d)) < epsilon = (val + d / val) / 2
                      | otherwise = babylon d epsilon ((val + d / val) / 2)