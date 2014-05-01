import Common


task = unlines [
  "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.",
  "Find the sum of all the multiples of 3 or 5 below 1000."
  ]

want :: Integer -> Bool
want n = (3 `divides` n) || (5 `divides` n)

solve :: Integer -> Integer
solve n = sum [x | x <- [1..n-1], want x]

main = runSolution task solve [10, 1000]
