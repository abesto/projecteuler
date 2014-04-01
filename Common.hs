module Common where

n `divides` k = (k `mod` n) == 0

divisibleBy = flip divides

isPalindrome n = (show n) == (reverse $ show n)

printSolution :: (Integer -> Integer) -> Integer -> IO ()
printSolution f n = do
  putStr $ show $ f n
  putStr " (N="
  putStr $ show n
  putStrLn ") "
