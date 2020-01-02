module Exercises (histogram) where

-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n xs 
    | n < 1 = error "n out of bounds"
    | otherwise = [sum[histogram'' xi xs | xi <- x] | x <- histogram' n xs]
        where  histogram' n xs = [[n*i..(n*(i+1) - 1)] | i <- [0..((maximum xs) `div` n)]]
               histogram'' x xs = length[x' | x' <- xs, x == x']
