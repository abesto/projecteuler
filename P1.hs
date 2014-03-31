import Common


task = unlines [
  "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.",
  "Find the sum of all the multiples of 3 or 5 below 1000."
  ]

want :: Integer -> Bool
want n = (3 `divides` n) || (5 `divides` n)

solve :: Integer -> Integer
solve n = sum [x | x <- [1..n-1], want x]

printSolution n = do
  putStr $ show $ solve n
  putStr " (N="
  putStr $ show n
  putStrLn ") "

main = do
  putStrLn $ task
  printSolution 10
  printSolution 1000
