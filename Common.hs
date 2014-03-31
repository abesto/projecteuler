module Common where

divides :: Integer -> Integer -> Bool
n `divides` k = (k `mod` n) == 0
