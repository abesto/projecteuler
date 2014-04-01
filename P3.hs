import Primes
import Common

task = unlines ["The prime factors of 13195 are 5, 7, 13 and 29."
               ,"What is the largest prime factor of the number 600851475143 ?"
               ]

solve n = head $ [x | x <- [xMax, xMax-1 .. 2], x `divides` n, isPrime x]
          where xMax = floor $ sqrt $ fromIntegral n

main = do
  putStrLn task
  printSolution solve 13195
  printSolution solve 600851475143
