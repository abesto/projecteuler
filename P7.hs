import Common
import Primes

task = unlines ["By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13."
               ,"What is the 10 001st prime number?"
               ]

solve n = head $ drop (n-1) $ naivePrimes

main = runSolution task solve [6, 10001]
