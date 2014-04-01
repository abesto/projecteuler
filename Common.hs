module Common where

divides :: Integer -> Integer -> Bool
n `divides` k = (k `mod` n) == 0

divisibleBy :: Integer -> Integer -> Bool
divisibleBy = flip divides

printSolution f n = do
  putStr $ show $ f n
  putStr " (N="
  putStr $ show n
  putStrLn ") "
