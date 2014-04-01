import Common
import Primes

task = unlines ["2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder."
               ,"What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
               ]

solve n = product $ commonPrimeFactors [2..n] $ sieveDesc n

commonPrimeFactors xs ps = foldl (mergeFactors ps) [] xs

mergeFactors ps fs x = foldl (withAtLeastMyCount myPrimeFactors) fs myPrimeFactors
  where myPrimeFactors = primeFactors x ps

count fs p = length $ filter (== p) fs
mustAdd myfs fs p = max 0 $ (count myfs p) - (count fs p)
withAtLeastMyCount myfs fs p = fs ++ (take (mustAdd myfs fs p) $ repeat p)

primeFactors 1 _  = []
primeFactors x pps@(p:ps)
  | p `divides` x = p:(primeFactors (x `div` p) pps)
  | otherwise     = primeFactors x ps

main = do
  putStrLn task
  printSolution solve 10
  printSolution solve 20
