module Primes (isPrime, sieveDesc, sieve) where

import Control.Parallel.Strategies
import Common


divisibleByAny :: [Integer] -> Integer -> Bool
divisibleByAny = flip $ any . divisibleBy

isPrime x = null $ [y | y <- [2 .. x `div` 2], x `divisibleBy` y]
naivePrimesBelow n = takeWhile (\x -> x < n) [x | x <- [2..], isPrime x]

parFilter c f xs = filter f xs `using` parListChunk c rseq

sieveDesc n = aux [x | x <- [2..n]] []  where
    aux [] primes = primes
    aux (p:ns) primes = aux (parFilter 4 (not . divisibleByAny (p:primes)) ns) (p:primes)
sieve = reverse . sieveDesc

main = do
  print $ sieve 100000
