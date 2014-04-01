import Common

task = unlines ["The sum of the squares of the first ten natural numbers is,"
               ,"12 + 22 + ... + 102 = 385"
               ,"The square of the sum of the first ten natural numbers is,"
               ,"(1 + 2 + ... + 10)2 = 552 = 3025"
               ,"Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640."
               ,"Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
               ]

solve n = ((sum xs) ^ 2) - (sum $ map (^2) xs)
          where xs = [1..n]

main = do
  putStrLn task
  printSolution solve 10
  printSolution solve 100
  putStrLn "Maybe I'm missing something, but this is plenty fast."
