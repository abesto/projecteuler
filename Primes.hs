module Primes (isPrime, sieveDesc, sieve, naivePrimes) where

import Control.Parallel.Strategies
import Common


divisibleByAny :: [Integer] -> Integer -> Bool
divisibleByAny = flip $ any . divisibleBy

isPrime x = null $ [y | y <- [2 .. (floor $ sqrt $ fromIntegral x)], x `divisibleBy` y]
naivePrimes = [x | x <- [2..], isPrime x]
naivePrimesBelow n = takeWhile (\x -> x < n) naivePrimes

parFilter c f xs = filter f xs `using` parListChunk c rseq

sieveDesc n = aux [x | x <- [2..n]] []  where
    aux [] primes = primes
    aux (p:ns) primes = aux (parFilter 4 (not . divisibleByAny (p:primes)) ns) (p:primes)
sieve = reverse . sieveDesc

main = do
  print $ sieve 100000
